package tui

import schema.ComparePlan
import schema.SchemaCompared._
import tui.layoutzEx.InputPrompt._
import tui.layoutzEx.JPromptShell._
import tui.layoutzEx._
import _root_.table.DiffRowSerDe
import utils.LogHelper.getFileSzList
import zio.Runtime

import java.nio.file.{Files, Paths}
import scala.Console.println

object SyncTUI {

  implicit val sema: ScreenSemaphore = ScreenSemaphore()
  val bullet = "> ".color(Color.Yellow).render

  val bannerElement = {
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

  // --------------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {

    val terminal = JLineTerminalWrapper.create().toOption
    if(terminal.isEmpty)
      println(bullet + "Terminal Driver not found.")

    JPromptShell.promptShell(terminal.get, "DBSync/")(
      new JPromptShell.ShellHandler{

        private var tuiConfState : TUIConfState = null
        private var tuiJobState : Option[TUIJob[ComparePlan]] = None

        def tuiConf(implicit show: String => Unit, inst: RuntimeShellInstance): TUIConfState = {
          if(tuiConfState == null) tuiConfState = TUIConfState(show, inst)
          tuiConfState
        }

        def jobHaveResult(implicit show: String => Unit, rt: Runtime[Any]) = {
          tuiJobState.exists(j => j.isRunning || j.isDone)
        }

        def jobIsRunning(implicit show: String => Unit, rt: Runtime[Any]) = {
          val out = tuiJobState.exists(_.isRunning)
          if(out)
            show(bullet + s"Job(${tuiJobState.get.name}) is running.")
          out
        }

        def jobIsDone(implicit show: String => Unit, rt: Runtime[Any]) = {
          val out = tuiJobState.exists(_.isDone)
          if(out)
            show(bullet + s"Job(${tuiJobState.get.name}) is done. see result with " + "jd".color(Color.Green).render)
          out
        }

        def getNotExistingPath(getInput: () => String)(implicit show: String => Unit) = {
          var path = getInput()
          while (Files.exists(Paths.get("jcf_"+ path))) {
            show("\n" + bullet + s"$path already exist.")
            path = getInput()
          }
          path
        }
        def makeJob(plans: Option[List[ComparePlan]], jobName: Option[String] = None)
                   (implicit show: String => Unit, inst: RuntimeShellInstance)
        : Option[TUIJob[ComparePlan]] = {

          plans.flatMap{ ps =>
            if (ps.isEmpty) {
              show(bullet + s"empty plan.")
              None
            } else {
              val jname = jobName.getOrElse( getNotExistingPath(() =>
                  readNotEmpty( inst.term,
                    "", "? job name(use as legal filename) ? ".color(Color.Yellow).render,
                    Some("myJob"), false)
                ) )
              val out = TUIJob[ComparePlan](show, inst, jname)
              for (p <- ps) {
                out.add(p)
              }
              show(bullet + s"job(${jname.color(Color.Yellow).render}) with ${ps.size} tables.")
              Some(out)
            }
          }
        }

        def withJob( f: TUIJob[ComparePlan] => Unit)(implicit show: String => Unit, inst: RuntimeShellInstance): Unit
        = tuiJobState match {
            case Some(s) => f(s)
            case None => show(bullet + "no job. make new with " + "jn".color(Color.Green).render)
          }

        def listJob(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]) {
          withJob{ j =>
            val ps = j.list()
            show(bullet + s"job(${j.name.color(Color.Yellow).render}) : tables")
            show( tableOfInfos( ps.map(_._2.table).toSeq) )

            if(j.isRunning) {
              show(bullet + s"state: Running. ses detail with " + "jd".color(Color.Green).render)
            } else if(j.isDone) {
              show(bullet + s"state: Done. ses detail with " + "jd".color(Color.Green).render)
            }
          }
        }

        def delJob(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]) {
          withJob{ j =>
            if(j.isRunning) {
              show(bullet + s"try after job(${j.name.color(Color.Yellow).render}) done.")
            } else if(j.isDone) {
              show(bullet + s"previous job(${j.name.color(Color.Yellow).render}) is done. make new job with " +
                "jn".color(Color.Green).render)
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

        def insJob(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]) {
          withJob{ j =>
            if(j.isRunning) {
              show(bullet + s"try after job(${j.name.color(Color.Yellow).render}) done.")
            } else if(j.isDone) {
              show(bullet + s"previous job(${j.name.color(Color.Yellow).render}) is done. make new job with " +
                "jn".color(Color.Green).render)
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

        def newJob(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]) {
          tuiJobState match {
            case Some(j) =>
              if(j.isRunning) {
                show(bullet + s"try after job(${j.name}) done.")
              } else {
                val confirm = {
                  if (j.isDone) {
                    show(bullet + s"previous job-result exists. see result with " + "jd".color(Color.Green).render)
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

        def detailJob(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]): Unit = {
          withJob{ j =>
            if(jobHaveResult) {
              val oss = j.getSnapshot
              if(oss.isEmpty){
                show(bullet + "i can't get job state. retry..")
                return
              }
              val jl = j.list()
              show(s"----- ${j.name} : tables (${jl.size})".color(Color.Green).render)

              val ss = j.getSnapshot.get

              val grouped = ss.reports.groupBy(_._2.state)
              show( s"----- ${j.name} : current progress (${ss.state})".color(Color.Cyan).render)
              grouped.foreach{ case (st, vals) =>
                show( vals.map{case (n, rm) => rm.statusString }
                  .mkString(s"@ $st (${vals.size})\n  ".color(Color.Cyan).render, "\n  ", ""))
              }
            }
          }
        }

        def stopAllJob(mode: StopMode)(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]): Unit = {
          withJob{ j =>
            if (j.isRunning) {
              val confirm = askConfirm(inst.term,
                mode match {
                  case SM_all => bullet + "stop " + "all & waiting".color(Color.Green).render + " tasks.\n"
                  case SM_activeAll => bullet + "stop " + "current active".color(Color.Green).render + " tasks.\n"
                  case SM_select => bullet + "command error. please report to me.\n"
                })
              if (confirm) j.stopAsync(rt, mode)
            } else {
              show(bullet + "no job is running.")
            }
          }
        }

        def stopJobs(names: List[String])(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]): Unit = {
          if(names.isEmpty) show( bullet + "input table-name to stop.")
          else withJob{ j =>
            if (j.isRunning) {
              names.foreach(n => j.stopAsync(rt, SM_select, n))
            } else {
              show(bullet + "no job is running.")
            }
          }
        }

        def selectJcfPath()(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any])
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
              e => { show(bullet + s"fail to select path: ${e.toString}"); None},
              r => Some(r.selectedItem)
            )

          jcf
        }

        def selectJcfFile(path: String)(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any])
        : Option[String] = {

          val list = getFileSzList("", ".msgpack", path).fold(
            e => { show(bullet + s"fail to find jcf-outs: ${e.getMessage}"); List.empty},
            r => r.filter(_._2 > 0)
          )
          show(bullet + s"${list.size} files exist.")
          val ret = SingleBox
            .singleBox("select jcf file", list.map(fs => fs._1 ))
            .run( clearOnStart = false, clearOnExit = false, terminal = Some(inst.term))
            .fold(
              e => {show(bullet + s"fail to select : ${e.message}");None},
              r => Some(r.selectedItem)
            )
          ret
        }

        def selectJcfFiles(path: String)(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any])
        : List[String] = {

          val list = getFileSzList("", ".msgpack", path).fold(
            e => { show(bullet + s"fail to find jcf-outs: ${e.toString}"); List.empty},
            r => r )

          val ss: Set[Int] = MultiTable
            .multiTable("select jcf data", Seq("file", "bytes"), list.map(fs => Seq(fs._1, f"${fs._2}%13d")))
            .run( clearOnStart = false, clearOnExit = false, terminal = Some(inst.term))
            .fold(
              e => {show(bullet + s"fail to select : ${e.message}"); Set.empty},
              r => r.selected
            )
          val out = list.zipWithIndex.filter { case ((_, _), i) => ss.contains(i) }.map(_._1._1)
          out
        }

        def fileToView()(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any])
        : Unit = {

          val ot = for{
            path <- selectJcfPath()
            name <- selectJcfFile(path)
            full = Paths.get(path).resolve(name).toString
          } yield ( (path, name, full) )
          if(ot.isEmpty) return

          val (pn, fn, full) = ot.get

          show(bullet + "file: " + full.color(Color.Cyan).render)

          val ii = DiffRowSerDe.readDiffRows(fn, full, () => false, rm => show(rm.statusString))
            .fold(
              e => {show(bullet + e.getMessage); Iterator.empty},
              r => r )

          val skey = "s q ESC".color(Color.Yellow).render
          val okey = "other-key".color(Color.Yellow).render
          show(bullet + s"press ${skey} to stop, $okey(ex: space) to see next")
          ii.grouped(5).foreach{ dr =>
            val stop = readForStopOr(inst.term)
            if(stop) return
            dr.foreach(l => show(bullet + l.toString) )
          }
        }

        def fileToApplyJob(par: Int, mock: Boolean = false, debug: Boolean = false)(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any])
        : Unit = {
          if(jobIsRunning ) return
          if(tuiJobState.nonEmpty || tuiJobState.exists(_.isRunning)) {
            show(bullet + "previous job info will be cleared.")
          }

          val pathName = selectJcfPath() match {
            case Some(o) => o
            case None => return
          }

          val sc = tuiConf.load0(pathName, Some(pathName.drop(4))) match {
              case Some(out) =>
                if( !tuiConf.setConfIfChanged(out, ask = true)) return
                if( !tuiConf.connected) return
                out
              case None => return
          }

          val (job, sz) = {
            val names = selectJcfFiles(pathName).map(_.dropRight(8)) // .msgpack
            val filtered = sc.comparePlans.filter(n => names.contains(n.name) )
            val j = makeJob( Some(filtered), Some(pathName))
            if(names.isEmpty) return
            tuiJobState = j
            (j -> names.size)
          }

          val jfa = "File-to-apply".color(Color.Yellow).render
          val confirm = askConfirm(inst.term, bullet + s"File-to-apply start($sz tables) with $par threads.\n")

          if(confirm){

            val f = tuiConf.dataSourcesOr.map{ case (_, ds2) =>
              (cp:ComparePlan) => {
                val p = Paths.get(pathName)
                val full = p.resolve(cp.name + ".msgpack").toString
                cp.toApplyFromFile(ds2, full, mock = mock)
              }
            }

            if(par > 4) { show( bullet + s"$par is too many. i'll use 4 threads.(DBMS may overburden)") }
            val n = par.min(4)
            val st = job.get.startAsync(f.get, rt, n)
            if(st.isDefined) {
              inst.setBarStates(st.map(_.getRunningState))
              inst.showStatusBar()
            } else {
              show(bullet + s"fail to start $jfa with par($n)")
            }
          }
        }

        def compareToFileJob(par: Int)
                             (implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]): Unit = {
          withJob{ j =>

            if(j.isRunning) {
              show(bullet + s"try after job(${j.name}) done.")
              return
            } else if(j.isDone) {
              show(bullet + s"previous job(${j.name}) is done. see " + "jd jn".color(Color.Green).render)
              return
            }

            val path = "jcf_" + j.name
            val f = tuiConf.dataSourcesOr.map { case (ds1, ds2) =>
              (cp: ComparePlan) => {
                val p = Paths.get(path, cp.name + ".msgpack").toString
                cp.toCompareToFile(ds1, ds2, p)
              }
            }
            if(f.isEmpty) return

            val jcf = "jcf".color(Color.Green).render
            val sz = j.list().size
            val confirm = askConfirm(inst.term, bullet + s"Compare-to-file(${j.name.color(Color.Yellow).render}) start($sz tables) with $par threads.\n")
            if(confirm) {

              val dir = Files.createDirectories(Paths.get(".").resolve(path)) // create path

              show(bullet + s"result will be stored below path ${path.color(Color.Green).render}")
              if( !tuiConf.save(path, Some(j.name)) ) return

              if(par > 4) { show( bullet + s"$par is too many. i'll use 4 threads.(DBMS may overburden)") }
              val n = par.min(4)
              val st = j.startAsync(f.get, rt, n)
              if(st.isDefined) {
                inst.setBarStates(st.map(_.getRunningState))
                inst.showStatusBar()
              } else {
                show(bullet + s"fail to start $jcf with par($n)")
              }
            }
          }
        }

        def compareToApplyJob0( toMock: Boolean, par: Int)
                             (implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]): Unit = {
          withJob{ j =>

            if(j.isRunning) {
              show(bullet + s"try after job(${j.name}) done.")
              return
            } else if(j.isDone) {
              show(bullet + s"previous job(${j.name}) is done. see " + "jd jn".color(Color.Green).render)
              return
            }

            val f = tuiConf.dataSourcesOr.map{ case (ds1, ds2) => (_: ComparePlan).toCompareApplyTask(ds1, ds2, toMock) }
            if( f.isEmpty) return

            val sz = j.list().size
            val confirm = askConfirm(inst.term, bullet + s"Compare-and-Apply start($sz tables) with $par threads.\n")
            if(confirm) {
              if(par > 4) { show( bullet + s"$par is too many. i'll use 4 threads.(DBMS may overburden)") }
              val n = par.min(4)
              val st = j.startAsync(f.get, rt, n)
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
        }
        override def onExit(term: Terminal): Unit = {
          term.exitRawMode()
        }

        val promptDBSync = Some("DBSync/")
        // ----------------------------------------------
        def dontKnow(show: String => Unit, o: String) = {
          show(bullet + "sorry. i don't understand " + o.color(Color.Red).render)
          help(show)
        }
        val helpStr = "[hint]" +
          "\n" + bullet +"init   : so|source ta|target co|connect ia in|init cn|count sa|save lo|load".color(Color.BrightGreen).render +
          "\n" + bullet +"table  : br|brief mka mkb mca mcb oa ob l|list ln|lnokey lk|lkey".color(Color.BrightGreen).render +
          "\n" + bullet +"schema : d|def dn|dnokey dk|dkey mkad mkbd mcad mcbd oad obd".color(Color.BrightGreen).render +
          "\n" + bullet +"plan   : p|plan sw ps|dnokey ps[10] psa psd[n] pa[n] paa pad[a] padd[n]".color(Color.BrightGreen).render +
          "\n" + bullet +"job    : jn|jnew jl jlr|jld jla|jli js|jstop[name..] jsc jsa jd|jdetail" .color(Color.BrightGreen).render +
          "\n" + bullet +"       : jaa jaad jcf jfv jfa" .color(Color.BrightGreen).render
//          "\n" + bullet +"         jfa ".color(Color.Green).render

        def help(show: String => Unit) = show(helpStr)

        def toIntOr(s: Option[String], orElse: Int) = s.flatMap(_.toIntOption).getOrElse(orElse)

        def commandHandler(cmd: String, rest: List[String])(implicit show: String => Unit, inst: RuntimeShellInstance)
        : Option[String] = {

          implicit val zioRuntime: Runtime[Any] = Runtime.default

          cmd  match {
            case "so"| "source" => tuiConf.updateConSetting("source", isA = true)
            case "ta"| "target" => tuiConf.updateConSetting("target", isA = false)
            case "co"| "connect"=> tuiConf.connect()
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
            case "ln"| "lnokey" => tuiConf.show_ln
            case "lk"| "lkey"   => tuiConf.show_lk
            case "d" | "def"    => tuiConf.show_d
            case "dn"| "dnokey" => tuiConf.show_dn
            case "dk"| "dkey"   => tuiConf.show_dk
            case "mkad"         => tuiConf.show_mkad
            case "mkbd"         => tuiConf.show_mkbd
            case "mcad"         => tuiConf.show_mcad
            case "mcbd"         => tuiConf.show_mcbd
            case "oad"          => tuiConf.show_oad
            case "obd"          => tuiConf.show_obd
            case "p" | "plan"   => tuiConf.show_plan
            case "sw"           => tuiConf.setWhere     // <<<<< todo
            case "ps"           => tuiConf.start_ps(Some(toIntOr(rest.headOption, 10)))
            case "psa"          => tuiConf.start_ps(None)
            case "psd"          => tuiConf.start_ps(Some(toIntOr(rest.headOption, 5)), debug = true)
            case "pa"           => tuiConf.start_pa(Some(toIntOr(rest.headOption, 10)), mock = false)
            case "paa"          => tuiConf.start_pa(None, mock = false)
            case "pad"          => tuiConf.start_pa(Some(toIntOr(rest.headOption, 5)), compDebug = true, mock = false)
            case "padd"         => tuiConf.start_pa(Some(toIntOr(rest.headOption, 5)), compDebug = true, mock = true)

            case "jn"  | "jnew" => newJob
            case "jl"           => listJob
            case "jlr" | "jld"  => delJob
            case "jla" | "jli"  => insJob

            case "jsa"          => stopAllJob(SM_all)
            case "jsc"          => stopAllJob(SM_activeAll)
            case "js" |"jstop"  => stopJobs(rest)
            case "jd" |"jdetail"=> detailJob
            case "jcf"          => compareToFileJob(toIntOr(rest.headOption, 2))
            case "jfv"          => fileToView()
            case "jfa"          => fileToApplyJob(toIntOr(rest.headOption, 2))    // todo
            case "jfad"         => fileToApplyJob(toIntOr(rest.headOption, 2), mock = true, debug = false)    // todo
            case "jaa"          => compareToApplyJob0(toMock = false, toIntOr(rest.headOption, 2))
            case "jaad"         => compareToApplyJob0(toMock = true, toIntOr(rest.headOption, 2))

            case "h" | "help" | "hint" => help(show)
            case o                     => dontKnow(show, o.color(Color.Red).render)
          }
          promptDBSync
        }

        override def handleIO(prompt: String, show: String => Unit, line: String, inst: RuntimeShellInstance)
        : Option[String] = {

          implicit val show0: String => Unit = show
          implicit val inst0: RuntimeShellInstance = inst

          if(line.isEmpty) return None

          val token = line.trim.split("\\s+")
          val cmd = token(0).toLowerCase
          val rest: List[String] = if(token.isDefinedAt(1)) token.tail.toList else List.empty

          cmd match {
            case "exit" | "q" => show("see you again~~".color(Color.Cyan).render); exit = true
            case _            => commandHandler(cmd, rest)
          }
          None
        }
        override def stopIOLoop(): Boolean = exit
      }
    ).runShell()
  }
}
