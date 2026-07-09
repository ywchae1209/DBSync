package tui

import schema.ComparePlan
import schema.SchemaCompared._
import tui.layoutzEx.InputPrompt._
import tui.layoutzEx.JPromptShell._
import tui.layoutzEx._
import com.typesafe.scalalogging.Logger
import schema.ComparePlan.jobLogger
import _root_.table.{DiffApplier, DiffRow, DiffRowSerDe}
import utils.LogHelper.getFileSzList
import zio.Runtime
import utils.Implicits._

import java.io.{FileInputStream, InputStream}
import java.nio.file.{Files, Paths}
import scala.Console.println
import scala.util.{Failure, Success, Try}

object SyncTUI {

  val bullet: String = "> ".yellow
  val tuiLogger: Logger = Logger("DBSyncUI")
  private val bannerElement: Layout = {
    layout(
      "",
      "",
      box(" * * * ")(
        "",
        "Database Synchronizer".color(Color.Yellow).style(Style.Bold).center(),
        "",
        "╔╦╗┌─┐┌┬┐┌─┐┌┐ ┌─┐┌─┐┌─┐ ╔═╗┬ ┬┌┐┌┌─┐",
        " ║║├─┤ │ ├─┤├┴┐├─┤└─┐├┤  ╚═╗└┬┘││││  ",
        "╩╩╝┴ ┴ ┴ ┴ ┴└─┘┴ ┴└─┘└─┘ ╚═╝ ┴ ┘└┘└─┘",
        "",
        "ver 1.1 for oracle".color(Color.BrightBlack).center(),
        rowTight(bullet," project : ", "DeltaFlow".color(Color.Green) ," project"),
        rowTight(bullet," mission : ", "compare tables & synchronize"),
        rowTight(bullet," author  : ", "DeltaFlow team"),
        "",
        ""
      )
    )
  }

  ////////////////////////////////////////////////////////////////////////////////

  private case class TUIApp(jTerm: JLineTerminalWrapper)(implicit sema: ScreenSemaphore) extends ShellHandler {

    private val zioRuntime: Runtime[Any] = Runtime.default

    private var tuiConfState : TUIConfState = null
    private var tuiJobState : Option[TUIJob[ComparePlan]] = None

    private def display(s: String) = {
      sema.strictly{ () =>
        jTerm.writeLine(s)
        jTerm.flush()
      }
    }
    private def show(s: String): Unit = {
      display(s)
      tuiLogger.info("\n" + s)
    }

    private def tuiConf(implicit inst: RuntimeShellInstance): TUIConfState = {
      if(tuiConfState == null) tuiConfState = TUIConfState(show, display, inst)
      tuiConfState
    }

    private def jobHaveResult = {
      tuiJobState.exists(j => j.isRunning || j.isDone)
    }

    private def jobIsRunning = {
      val out = tuiJobState.exists(_.isRunning)
      if(out)
        show(bullet + s"Job(${tuiJobState.get.name}) is running.")
      out
    }

    private def getNotExistingPath(getInput: () => String) = {
      var path = getInput()
      while (Files.exists(Paths.get("jcf_"+ path))) {
        show("\n" + bullet + s"$path already exist.")
        path = getInput()
      }
      path
    }

    private def makeJob(plans: Option[List[ComparePlan]], jobName: Option[String] = None)
               (implicit inst: RuntimeShellInstance)
    : Option[TUIJob[ComparePlan]] = {

      plans.flatMap{ ps =>
        if (ps.isEmpty) {
          show(bullet + s"empty plan.")
          None
        } else {
          val jname = jobName.getOrElse( getNotExistingPath(() =>
            readNotEmpty( inst.term,
              "", "? job name(use as legal filename) ? ".yellow,
              Some("myJob"), false)
          ) )
          val out = TUIJob[ComparePlan](show, inst, jname)(zioRuntime)
          for (p <- ps) {
            out.add(p)
          }
          show(bullet + s"job(${jname.yellow}) with ${ps.size} tables.")
          Some(out)
        }
      }
    }

    private def withJob( f: TUIJob[ComparePlan] => Unit)(implicit inst: RuntimeShellInstance): Unit
    = tuiJobState match {
      case Some(s) => f(s)
      case None => show(bullet + "no job. make new with " + "jn".green)
    }

    private def listJob(implicit inst: RuntimeShellInstance) {
      withJob{ j =>
        val ps = j.list()
        show(bullet + s"job(${j.name.yellow}) : tables")
        show( tableOfInfos( ps.map(_._2.table).toSeq) )

        if(j.isRunning) {
          show(bullet + s"state: Running. ses detail with " + "jd".green)
        } else if(j.isDone) {
          show(bullet + s"state: Done. ses detail with " + "jd".green)
        }
      }
    }

    private def delJob(implicit inst: RuntimeShellInstance) {
      withJob{ j =>
        if(j.isRunning) {
          show(bullet + s"try after job(${j.name.yellow}) done.")
        } else if(j.isDone) {
          show(bullet + s"previous job(${j.name.yellow}) is done. make new job with " + "jn".green)
        } else {
          val ps = j.list()
          val tb = ps.map(_._2.table)
          implicit val iterm: Terminal = inst.term
          val ss = selectTables(tb.toSeq).map(_.name)
          if(ss.nonEmpty){
            j.removeIf(k => ss.contains(k))
            show(bullet + s"delete ${ss.size} tables from job(${j.name})")
            listJob
          } else
            show(bullet + s"nothing to delete.")
        }
      }
    }

    private def insJob(implicit inst: RuntimeShellInstance) {
      withJob{ j =>
        if(j.isRunning) {
          show(bullet + s"try after job(${j.name.yellow}) done.")
        } else if(j.isDone) {
          show(bullet + s"previous job(${j.name.yellow}) is done. make new job with " + "jn".green)
        } else {
          val ps0 = j.list()
          val ss = tuiConf.selectPlansNotIn(ps0.map(_._2.table.name).toSet)
          if(ss.nonEmpty) {
            ss.foreach(j.add)
            show(bullet + s"add ${ss.size} table to job(${j.name})")
          } else
            show(bullet + s"nothing left to add.")

        }
      }
    }

    private def newJob(implicit inst: RuntimeShellInstance) {
      tuiJobState match {
        case Some(j) =>
          if(j.isRunning) {
            show(bullet + s"try after job(${j.name}) done.")
          } else {
            val confirm = {
              if (j.isDone) {
                show(bullet + s"previous job-result exists. see result with " + "jd".green)
                askConfirm(inst.term, bullet + s"previous information will be cleared.\n")
              } else true
            }
            if (confirm) {
              tuiJobState = makeJob(tuiConf.selectPlansOr)
            }
          }
        case None => tuiJobState = makeJob(tuiConf.selectPlansOr)
      }
    }

    private def detailJob(implicit inst: RuntimeShellInstance): Unit = {
      withJob{ j =>
        if(jobHaveResult) {
          val oss = j.getSnapshot
          if(oss.isEmpty){
            show(bullet + "i can't get job state. retry..")
            return
          }
          val jl = j.list()
          show(s"----- ${j.name} : tables (${jl.size})".green)

          val ss = j.getSnapshot.get

          val grouped = ss.reports.groupBy(_._2.status)
          show( s"----- ${j.name} : current progress (${ss.state})".cyan)
          grouped.foreach{ case (st, vals) =>
            show( vals.map{case (n, rm) => rm.statusString }
              .mkString(s"@ $st (${vals.size})\n  ".cyan, "\n  ", ""))
          }
        }
      }
    }

    private def stopAllJob(mode: StopMode)(implicit inst: RuntimeShellInstance): Unit = {
      withJob{ j =>
        if (j.isRunning) {
          val confirm = askConfirm(inst.term,
            mode match {
              case SM_all => bullet + "stop " + "all & waiting".green + " tasks.\n"
              case SM_activeAll => bullet + "stop " + "current active".green + " tasks.\n"
              case SM_select => bullet + "command error. please report to me.\n"
            })
          if (confirm) j.stopAsync(mode)
        } else {
          show(bullet + "no job is running.")
        }
      }
    }

    private def stopJobs(names: List[String])(implicit inst: RuntimeShellInstance): Unit = {
      if(names.isEmpty) show( bullet + "input table-name to stop.")
      else withJob{ j =>
        if (j.isRunning) {
          names.foreach(n => j.stopAsync(SM_select, n))
        } else {
          show(bullet + "no job is running.")
        }
      }
    }

    private def selectJcfPath()(implicit inst: RuntimeShellInstance)
    : Option[String] = {

      val current = new java.io.File(".")

      val jcfs = current.listFiles().filter( f => f.isDirectory && f.getName.startsWith("jcf_")).map(_.getName)
      if(jcfs.isEmpty) {
        show(bullet + "no jcf out exists")
        return None
      }

      val jcf = SingleBox.singleBox("select jcf path", jcfs)
        .run( clearOnStart = false, clearOnExit = false, terminal = Some(inst.term))
        .fold(
          e => { show(bullet + s"fail to select path: ${e.getMessage}"); None},
          r => r.selectedItem
        )

      jcf
    }

    private def selectJcfFile(path: String)(implicit inst: RuntimeShellInstance)
    : Option[String] = {

      val list = getFileSzList("", ".msgpack", path).fold(
        e => { show(bullet + s"fail to find jcf-outs: ${e.getMessage}"); List.empty},
        r => {
          val emptyFiles = r.filter(_._2 == 0)
          if(emptyFiles.nonEmpty) {
            show( bullet + s"empty files(${emptyFiles.size.toString.green}) exist")
            display( emptyFiles.map( _._1.green ).mkString("\t", "\n\t", "\n" ))
          }
          r.filter(_._2 > 0)
        }
      )
      if(list.isEmpty) {
        show(bullet + "no file to view")
        None
      } else {
        val ret = SingleBox
          .singleBox("select jcf file", list.map(fs => fs._1 ))
          .run( clearOnStart = false, clearOnExit = false, terminal = Some(inst.term))
          .fold(
            e => {show(bullet + s"fail to select : ${e.getMessage}");None},
            r => r.selectedItem
          )
        ret
      }
    }

    private def selectJcfFiles(path: String)(implicit inst: RuntimeShellInstance)
    : List[String] = {

      val list = getFileSzList("", ".msgpack", path).fold(
        e => { show(bullet + s"fail to find jcf-outs: ${e.getMessage}"); List.empty},
        r => r )

      val ss: Set[Int] = MultiTable
        .multiTable("select jcf data", Seq("file", "bytes"), list.map(fs => Seq(fs._1, f"${fs._2}%13d")))
        .run( clearOnStart = false, clearOnExit = false, terminal = Some(inst.term))
        .fold(
          e => {show(bullet + s"fail to select : ${e.getMessage}"); Set.empty},
          r => r.selected
        )
      val out = list.zipWithIndex.filter { case ((_, _), i) => ss.contains(i) }.map(_._1._1)
      out
    }

    private def jcfStream(prefix: String)(implicit inst: RuntimeShellInstance)
    : Option[(String, String, String, Iterator[(Int, DiffRow)] with AutoCloseable)]
    = {
      val ot = for{
        pn <- selectJcfPath()
        fn <- selectJcfFile(pn)
        full = Paths.get(pn).resolve(fn).toString
      } yield ( (pn, fn, full) )

      ot.map{ case (_, _, full) =>
        val (pn, fn, full) = ot.get

        show(bullet + prefix + full.cyan)

        val ret = DiffRowSerDe.readDiffRows(full)
          .fold(
            e => { display(bullet + e.getMessage); emptyIterator },
            r => r.zipIndexFrom(1))

        (pn, fn, full, ret)
      }
    }

    private def fileToView(o: Option[String])(implicit inst: RuntimeShellInstance)
    : Unit = {
      jcfStream("view file: ").foreach{ case (pn, fn, full, js) =>

        val os = o.flatMap{ _ =>
          val full1 = full + ".dat"
          Try { new FileInputStream(full1) } match {
            case Failure(e) => show(bullet + s"can't read ${full1.green} (${e.getMessage})"); None
            case Success(v) => Some(v)
          }
        }

        val skey = "s q ESC".yellow
        val okey = "other-key".yellow
        display(bullet + s"press ${skey} to stop, $okey(ex: space) to see next")

        val js0 = os.map( is => DiffApplier.extractDiffs(js, is, None)).getOrElse(js)
        js0.grouped(5).foreach{ drs =>
          val stop = readForStopOr(inst.term)
          if(stop) return
          drs.foreach{ case (n, l) => display(bullet + n.toString.green + " : " + l.toPretty) }
        }
        show(bullet + "done(file view):" + full.cyan)
        js.close()
      }
    }

    private def fileApplyFrom(offset: Int, kind: Option[String] = None)(implicit inst: RuntimeShellInstance): Unit = {
      jcfStream(s"apply after ${offset}").foreach { case (pn, fn, full, js) =>

        val sc = tuiConf.load0(pn, Some(pn.drop(4))) match {
          case Some(out) =>
            if( !tuiConf.setConfIfChanged(out, ask = true)) return
            if( !tuiConf.connected) return
            out
          case None => return
        }

        val cp = {
          val tableName = fn.dropRight(8)
          val out = sc.comparePlans.find(_.name == tableName)
          if(out.isEmpty){
            show(bullet + s"error : ${tableName.green} not found in conf.")
            return
          }
          out.get
        }

        val kindPred = tuiConf.kindFilter(kind)
        val f = tuiConf.comparedDataSourcesOr.map{ case (_, ds2) =>
          cp.toApplyFromFile(ds2, full, kindPred, offset = Some(offset))
        }

        try{
          f.foreach( _.go(()=> false, rm => display(rm.statusString)) )
        } finally {
          js.close()
        }
      }
    }

    private def fileToApplyJob(par: Int, kind: Option[String] = None, mock: Boolean = false, applDebug: Boolean = false)(implicit inst: RuntimeShellInstance)
    : Unit = {
      if(jobIsRunning ) return
      if(tuiJobState.nonEmpty || tuiJobState.exists(_.isRunning)) {
        show(bullet + "previous job info will be cleared.")
      }

      val pn = selectJcfPath() match {
        case Some(o) => o
        case None => return
      }

      val sc = tuiConf.load0(pn, Some(pn.drop(4))) match {
        case Some(out) =>
          if( !tuiConf.setConfIfChanged(out, ask = true)) return
          if( !tuiConf.connected) return
          out
        case None => return
      }

      val (job, sz) = {
        val names = selectJcfFiles(pn).map(_.dropRight(8)) // .msgpack
        val filtered = sc.comparePlans.filter(n => names.contains(n.name) )
        val j = makeJob( Some(filtered), Some(pn))
        if(names.isEmpty) return
        tuiJobState = j
        (j -> names.size)
      }

      val jfa = "file-to-apply".yellow
      val kindPred = tuiConf.kindFilter(kind)
      val confirm = askConfirm(inst.term, bullet + s"File-to-apply start($sz tables) with $par threads.\n")

      if(confirm){

        val f = tuiConf.comparedDataSourcesOr.map{ case (_, ds2) =>
          (cp:ComparePlan) => {
            val p = Paths.get(pn)
            val full = p.resolve(cp.name + ".msgpack").toString
            cp.toApplyFromFile(ds2, full, kindPred, mock = mock, applDebug= applDebug)
          }
        }

        if(par > 4) { show( bullet + s"$par is too many. i'll use 4 threads.(DBMS may overburden)") }
        val n = par.min(4)
        val st = job.get.startAsync(f.get, n)
        if(st.isDefined) {
          inst.setBarStates(st.map(_.getRunningState))
          inst.showStatusBar()
        } else {
          show(bullet + s"fail to start $jfa with par($n)")
        }
      }
    }

    private def compareToFileJob(par: Int)(implicit inst: RuntimeShellInstance): Unit = {
      withJob{ j =>

        if(j.isRunning) {
          show(bullet + s"try after job(${j.name.yellow}) done.")
          return
        } else if(j.isDone) {
          show(bullet + s"previous job(${j.name.yellow}) is done. see " + "jd jn".green)
          return
        }

        val path = "jcf_" + j.name
        val f = tuiConf.comparedDataSourcesOr.map { case (ds1, ds2) =>
          (cp: ComparePlan) => {
            val p = Paths.get(path, cp.name + ".msgpack").toString
            cp.toCompareToFile(ds1, ds2, p)
          }
        }
        if(f.isEmpty) return

        val jcf = "jcf".green
        val sz = j.list().size
        val confirm = askConfirm(inst.term, bullet + s"Compare-to-file(${j.name.yellow}) start($sz tables) with $par threads.\n")
        if(confirm) {

          val dir = Files.createDirectories(Paths.get(".").resolve(path)) // create path

          show(bullet + s"result will be stored below path ${path.green}")
          if( !tuiConf.save(path, Some(j.name)) ) return

          if(par > 4) { show( bullet + s"$par is too many. i'll use 4 threads.(DBMS may overburden)") }
          val n = par.min(4)
          val st = j.startAsync(f.get, n)
          if(st.isDefined) {
            inst.setBarStates(st.map(_.getRunningState))
            inst.showStatusBar()
          } else {
            show(bullet + s"fail to start $jcf with par($n)")
          }
        }
      }
    }

    private def compareToApplyJob0(par: Int, debug: Boolean = false, mock: Boolean = false)(implicit inst: RuntimeShellInstance): Unit = {
      withJob{ j =>

        if(j.isRunning) {
          show(bullet + s"try after job(${j.name}) done.")
          return
        } else if(j.isDone) {
          show(bullet + s"previous job(${j.name}) is done. see " + "jd jn".green)
          return
        }

        val f = tuiConf.comparedDataSourcesOr.map{ case (ds1, ds2) => (_: ComparePlan).toCompareApplyTask(ds1, ds2, mock) }
        if( f.isEmpty) return

        val sz = j.list().size
        val confirm = askConfirm(inst.term, bullet + s"Compare-and-Apply start($sz tables) with $par threads.\n")
        if(confirm) {
          if(par > 4) { show( bullet + s"$par is too many. i'll use 4 threads.(DBMS may overburden)") }
          val n = par.min(4)
          val st = j.startAsync(f.get, n)
          if(st.isDefined) {
            inst.setBarStates(st.map(_.getRunningState))
            inst.showStatusBar()
          } else {
            show(bullet + s"fail: start to compare-apply with par($n)")
          }
        }
      }
    }

    // ----------------------------------------------
    @volatile var exit: Boolean = false

    override def onStart(term: Terminal): Unit = {
      term.enterRawMode()
      bannerElement.render.split("\\n").foreach { term.writeLine }
      jobLogger.info("---------- DBSync started ----------".cyan)
    }
    override def onExit(term: Terminal): Unit = {
      term.exitRawMode()
    }

    // ----------------------------------------------
    private def dontKnow(show: String => Unit, o: String) = {
      display(bullet + "sorry. i don't understand " + o.red)
      help()
    }
    val helpStr = "[hint]" +
      "\n" + bullet +"init   : so|source ta|target st co|connect in|init ia cn|count sa|save lo|load".brightGreen +
      "\n" + bullet +"table  : br|brief mka mkb mca mcb oa ob l|list lst ln lk".brightGreen +
      "\n" + bullet +"schema : d|def dn dk mkad mkbd mcad mcbd oad obd".brightGreen +
      "\n" + bullet +"plan   : p|plan pst sw ps[10] psa psd[n] pa[n] paa[iud] pad[n] pam[n]".brightGreen +
      "\n" + bullet +"job    : jn|jnew jl jlr|jld jla|jli jsa jsc js[name] jd" .brightGreen +
      "\n" + bullet +"       : ja jad jam jcf jfv[-d] jfa jfad jfac[off] jfak[iud][pn] jfam" .brightGreen

    def help() = display(helpStr)

    private def toIntOr(s: Option[String], orElse: Int): Int = {
      s.flatMap(_.toIntOption).getOrElse{ orElse }
    }

    private def commandHandler(cmd: String, opts: List[String])(implicit inst: RuntimeShellInstance)
    : Unit = {

      tuiLogger.info(bullet + "(user) "+ opts.mkString(cmd, " ", "").green)
      val rest = opts.lift
      cmd  match {
        case "so"| "source" => tuiConf.updateConSetting("source", isA = true)
        case "ta"| "target" => tuiConf.updateConSetting("target", isA = false)
        case "st"           => tuiConf.show_st
        case "co"| "connect"=> tuiConf.connect()
        case "cst"          => tuiConf.show_cst
        case "in"| "init"   => tuiConf.start_init(all = false)
        case "ia"           => tuiConf.start_init(all = true)
        case "cn"| "count"  => tuiConf.updateCount()
        case "sa"| "save"   => tuiConf.save()
        case "lo"| "load"   => tuiConf.load()
        case "br"| "brief"  => tuiConf.show_b
        case "mka"          => tuiConf.show_mka
        case "mkb"          => tuiConf.show_mkb
        case "mca"          => tuiConf.show_mca
        case "mcb"          => tuiConf.show_mcb
        case "oa"           => tuiConf.show_oa
        case "ob"           => tuiConf.show_ob
        case "l" | "list"   => tuiConf.show_l
        case "pst"          => tuiConf.show_pst
        case "ln"           => tuiConf.show_ln
        case "lk"           => tuiConf.show_lk
        case "d" | "def"    => tuiConf.show_d
        case "dn"           => tuiConf.show_dn
        case "dk"           => tuiConf.show_dk
        case "mkad"         => tuiConf.show_mkad
        case "mkbd"         => tuiConf.show_mkbd
        case "mcad"         => tuiConf.show_mcad
        case "mcbd"         => tuiConf.show_mcbd
        case "oad"          => tuiConf.show_oad
        case "obd"          => tuiConf.show_obd
        case "p" | "plan"   => tuiConf.show_plan
        case "sw"           => tuiConf.setWhere
        case "ps"           => tuiConf.start_ps(Some(toIntOr(rest(0), 10)))
        case "psa"          => tuiConf.start_ps(None)
        case "psd"          => tuiConf.start_ps(Some(toIntOr(rest(0), 5)), debug = true)
        case "pa"           => tuiConf.start_pa(Some(toIntOr(rest(0), 10)))
        case "paa"          => tuiConf.start_pa(None, kind = rest(0))
        case "pad"          => tuiConf.start_pa(Some(toIntOr(rest(0), 5)), compDebug = true, applDebug= true)
        case "pam"          => tuiConf.start_pa(Some(toIntOr(rest(0), 5)), compDebug = true, applDebug= true , mock = true)
        case "jn"  | "jnew" => newJob
        case "jl"           => listJob
        case "jlr" | "jld"  => delJob
        case "jla" | "jli"  => insJob
        case "jsa"          => stopAllJob(SM_all)
        case "jsc"          => stopAllJob(SM_activeAll)
        case "js"           => stopJobs(opts)
        case "jd"           => detailJob
        case "ja"           => compareToApplyJob0(toIntOr(rest(0), 2))
        case "jad"          => compareToApplyJob0(toIntOr(rest(0), 2), debug = true)
        case "jam"          => compareToApplyJob0(toIntOr(rest(0), 2), debug = true, mock = true)
        case "jcf"          => compareToFileJob(toIntOr(rest(0), 2))
        case "jfv"          => fileToView(rest(0).filter(_ == "-d"))
        case "jfa"          => fileToApplyJob(toIntOr(rest(0), 2))
        case "jfac"         => fileApplyFrom(toIntOr(rest(0), 1), rest(1))
        case "jfak"         => fileToApplyJob(toIntOr(rest(1), 2), kind = rest(0))
        case "jfad"         => fileToApplyJob(toIntOr(rest(0), 2), applDebug = true)
        case "jfam"         => fileToApplyJob(toIntOr(rest(0), 2), applDebug = true, mock = true)

        case "h" | "help" | "hint" => display(fullHelp)
        case o               => dontKnow(show, o.color(Color.Red).render)

      }
    }
    val fullHelp = List(
      "so |source"     -> "set source DB connect setting",
      "ta |target"     -> "set target DB connect setting",
      "st"             -> "current db-conf",
      "co |connect"    -> "make connection",
      "cst"            -> "current connected db-conf",
      "in |init"       -> "load to make compare-plan for selection",
      "ia"             -> "load to make compare-plan",
      "cn |count"      -> "fetch row-counts",
      "sa |save"       -> "save conf",
      "lo |load"       -> "load conf",
      "br |brief"      -> "brief compare-plan",
      "mka"            -> "list mismatch-key tables of source",
      "mkb"            -> "list mismatch-key tables of target",
      "mca"            -> "list mismatch-col tables of source",
      "mcb"            -> "list mismatch-col tables of target",
      "oa"             -> "list tables only in source",
      "ob"             -> "list tables only in target",
      "l |list"        -> "list comparable tables",
      "ln"             -> "list comparable no-key-existing tables",
      "lk"             -> "list comparable key-existing tables",
      "d |def"         -> "definition of table",
      "dn"             -> "definition of no-key-existing table",
      "dk"             -> "definition of key-existing table",
      "mkad"           -> "definition of mismatch-key table of source",
      "mkbd"           -> "definition of mismatch-key table of target",
      "mcad"           -> "definition of mismatch-col table of source",
      "mcbd"           -> "definition of mismatch-col table of target",
      "oad"            -> "definition of table only in source",
      "obd"            -> "definition of table only in target",
      "p |plan"        -> "see SQL used to compare and apply",
      "pst"            -> "plan application db-conf",
      "sw"             -> "set where clause to select sql",
      "ps [n]"         -> "(plan)sample compare. default n=10",
      "psa"            -> "(plan) compare all",
      "psd [n]"        -> "(plan) compare with logging(to log-file). n=5",
      "pa [n]"         -> "(plan) compare and apply. default n= 10",
      "paa [iud]"      -> "(plan) compare and apply all. iud=kind to apply",
      "pad"            -> "(plan) paa with debug. n=5",
      "pam"            -> "(plan) pad but, appply to mock-up(not target). n=5",
      "jn | jnew"      -> "(job) make new job.(will run in multi-thread)",
      "jl"             -> "(job) list",
      "jlr | jld"      -> "(job) list remove",
      "jla | jli"      -> "(job) list add",
      "jsa"            -> "(job) stop all-tasks of running job",
      "jsc"            -> "(job) stop current-tasks of running job",
      "js [name..]"    -> "(job) stop tasks by names",
      "jd"             -> "(job) running or done result.",
      "ja [pn]"        -> "(job) compare and apply with pn thread. pn=2",
      "jad [pn]"       -> "(job) pa with logging (to log-file)",
      "jam [pn]"       -> "(job) pad but, apply to mock-up(not target)",
      "jcf [pn]"       -> "(job) compare to file",
      "jfv [-d]"       -> "(job) view stored file",
      "jfa [pn]"       -> "(job) file to apply",
      "jfac [off][iud]"-> "(job) file to apply. continue after offset",
      "jfak [iud] [pn]"-> "(job) file to apply. selected kind(insert, update, delete).",
      "jfad [pn]"      -> "(job) file to apply with logging(to log-file)",
      "jfam [pn]"      -> "(job) jfad but apply to mock-up(not-target)",

    ).map{case (c, d) => bullet + f"${c}%-17s".green + d}.mkString("\n")

    override def handleIO(prompt: String, line: String, inst: RuntimeShellInstance) : Unit = {

      implicit val inst0: RuntimeShellInstance = inst

      if(line.isEmpty) return

      val token = line.trim.split("\\s+")
      val cmd = token(0).toLowerCase
      val rest: List[String] = if(token.isDefinedAt(1)) token.tail.toList else List.empty

      cmd match {
        case "exit" | "q" =>
          display("see you again~~".cyan);
          jobLogger.info("---------- DBSync ended   ----------".cyan)
          exit = true
        case _            => commandHandler(cmd, rest)
      }
    }
    override def stopIOLoop(): Boolean = exit
  }

  // --------------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {

    val terminal = JLineTerminalWrapper.create().toOption

    terminal match {
      case None       => println(bullet + "Terminal Driver not found.")
      case Some(jTermWrapper) =>
        val prompt = "DBSync/".yellow

        val sema = ScreenSemaphore()
        val app = TUIApp(jTermWrapper)(sema)

        promptShell(jTermWrapper, prompt, app, sema).runShell()
    }
  }
}
