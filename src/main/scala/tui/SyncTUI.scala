package tui

import schema.ComparePlan
import schema.SchemaCompared._
import tui.layoutzEx.InputPrompt._
import tui.layoutzEx.JPromptShell._
import tui.layoutzEx._
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

        def showJobRunning(implicit show: String => Unit) = show(bullet + "make a job first.")
        def showJobNotRunning(implicit show: String => Unit) = show(bullet + "No job is running.")
        def jobIsRunning(implicit show: String => Unit, rt: Runtime[Any]) = {
          val out = tuiJobState.exists(_.isRunning)
          if(out)
            show(bullet + s"job (${tuiJobState.get.name}) is running.")
          out
        }

        def makeJob()(implicit show: String => Unit, inst: RuntimeShellInstance)
        : Option[TUIJob[ComparePlan]] = {

          tuiConf.selectPlansOr.flatMap{ ps =>
            if (ps.isEmpty) {
              show(bullet + s"new job is not created.")
              None
            } else {
              val jname = readNotEmpty( inst.term, "", "? job name(use as legal filename) ? ".color(Color.Red).render, Some("myJob"), false)
              val out = TUIJob[ComparePlan](show, inst, jname) //, p => TUITask(p.name)) // todo ::
              for (p <- ps) {
                out.add(p)
              }
              show(bullet + s"new job(${jname}) is created with ${ps.size} tables.")
              Some(out)
            }
          }
        }

        def withJob( f: TUIJob[ComparePlan] => Unit)(implicit show: String => Unit, inst: RuntimeShellInstance): Unit
        = tuiJobState match {
            case Some(s) => f(s)
            case None => showJobRunning
          }

        def listJob(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]) {
          withJob{ j =>
            val ps = j.list()
            show(bullet + "Job List : tables")
            show( tableOfInfos( ps.map(_._2.table).toSeq) )
            if(j.isRunning) showJobRunning
          }
        }

        def delJob(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]) {
          withJob{ j =>
            if(j.isRunning) showJobRunning else {
              val ps = j.list()
              val tb = ps.map(_._2.table)
              implicit val iterm: Terminal = inst.term
              val ss = selectTables(tb.toSeq).map(_.name)
              j.removeIf(k => ss.contains(k))
              show( bullet + s"delete ${ss.size} tables from job ")
              listJob
            }
          }
        }

        def insJob(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]) {
          withJob{ j =>
            if(j.isRunning) showJobRunning else {
              val ps0 = j.list()
              val ss = tuiConf.selectPlansNotIn(ps0.map(_._2.table.name).toSet)
              ss.foreach(j.add)
              show(bullet + s"insert ${ss.size} table to job")
            }
          }
        }

        def newJob(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]) {
          tuiJobState match {
            case Some(j) =>
              if (!jobIsRunning) {
                val confirm = askConfirm(inst.term, bullet + s"Job(${j.name}) list(not-running) will be cleared\n")
                if (confirm) {
                  tuiJobState = makeJob()
                }
              }
            case None =>
              tuiJobState = makeJob()
          }
        }

        def detailJob(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]): Unit = {
          withJob{ j =>
            val oss = j.getSnapshot
            if(oss.isEmpty){
              show(bullet + "sorry. i can't get job state.(may not in progress) retry..")
              return
            }
            val jl = j.list()
            show( jl.keys.mkString( s"----- tasks in job (${jl.size})\n  ".color(Color.Green).render, "\n  ", "" ) )

            val ss = j.getSnapshot.get

            val grouped = ss.reports.groupBy(_._2.state)
            show( s"----- current progress (${ss.state})".color(Color.Cyan).render)
            grouped.foreach{ case (st, vals) =>
              show( vals.map{case (n, rm) => rm.statusString }
                .mkString(s"@ $st (${vals.size})\n  ".color(Color.Cyan).render, "\n  ", ""))
            }
          }
        }
        def stopAllJob(mode: StopMode)(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]): Unit = {
          withJob{ j =>
            if(!j.isRunning) showJobNotRunning else {
              val confirm = askConfirm(inst.term,
                mode match {
                  case SM_all       => bullet+"stop " + "all & waiting".color(Color.Green).render + " tasks.\n"
                  case SM_activeAll => bullet+"stop " + "current active".color(Color.Green).render + " tasks.\n"
                  case SM_select    => bullet+"command error. please report to developer.\n"
                } )
              if(confirm) j.stopAsync(rt, mode)
            }
          }
        }

        def stopJobs(names: List[String])(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]): Unit = {
          if(names.isEmpty) show( bullet + "input task to stop.")
          else withJob{ j =>
            if(!j.isRunning) showJobNotRunning else {
              names.foreach ( n => j.stopAsync(rt, SM_select, n) )
            }
          }
        }

        def fileToView()(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any])
        : Unit = {
          val current = new java.io.File(".")
          val jcfs = current.listFiles().filter( f => f.isDirectory && f.getName.startsWith("jcf_")).map(_.getName)
          if(jcfs.isEmpty) return

          val jcf = SingleBox.singleBox("select jcf path", jcfs)
            .run( clearOnStart = false, clearOnExit = false, terminal = Some(inst.term))
            .fold(
              e => { show(bullet + s"fail to select path: ${e.toString}"); None},
              r => Some(r.selectedItem)
            )

          if(jcf.isEmpty) return

          val list = getFileSzList("", ".msgpack", jcf.get).fold(
            e => { show(bullet + s"fail to find jcf-outs: ${e.toString}"); List.empty},
            r => r
          )

          val n = SingleBox
            .singleBox("select jcf data", list.map(fs => fs._1))
            .run( clearOnStart = false, clearOnExit = false, terminal = Some(inst.term))

        }

        def fileToApplyJob(par: Int)(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any])
        : Unit = {

          val current = new java.io.File(".")
          val jcfs = current.listFiles().filter( f => f.isDirectory && f.getName.startsWith("jcf_")).map(_.getName)
          if(jcfs.isEmpty) return

          val jcf = SingleBox.singleBox("select jcf path", jcfs)
            .run( clearOnStart = false, clearOnExit = false, terminal = Some(inst.term))
            .fold(
              e => { show(bullet + s"fail to select path: ${e.toString}"); None},
              r => Some(r.selectedItem)
            )

          if(jcf.isEmpty) return

          val list = getFileSzList("", ".msgpack", jcf.get).fold(
            e => { show(bullet + s"fail to find jcf-outs: ${e.toString}"); List.empty},
            r => r
          )

          val n = MultiTable
            .multiTable("select jcf data", Seq("file", "bytes"), list.map(fs => Seq(fs._1, f"${fs._2}%13d")))
            .run( clearOnStart = false, clearOnExit = false, terminal = Some(inst.term))
        }


        def compareToFileJob(path: String, par: Int)
                             (implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]): Unit = {
          withJob{ j =>

            val f = tuiConf.dataSourcesOr.map { case (ds1, ds2) =>
              (cp: ComparePlan) => {
                val p = Paths.get(path, cp.name + ".msgpack").toString
                cp.toCompareToFile(ds1, ds2, p)
              }
            }
            if(f.isEmpty) return

            val sz = j.list().size
            val confirm = askConfirm(inst.term, bullet + s"Job(Compare to file) start($sz tables) with $par threads.\n")
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

        def compareToApplyJob0( toMock: Boolean, par: Int)
                             (implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]): Unit = {
          withJob{ j =>

            val f = tuiConf.dataSourcesOr.map{ case (ds1, ds2) => (_: ComparePlan).toCompareApplyTask(ds1, ds2, toMock) }
            if( f.isEmpty) return

            val sz = j.list().size
            val confirm = askConfirm(inst.term, bullet + s"Job(Compare & Apply) start($sz tables) with $par threads.\n")
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
          "\n" + bullet +"init   : so|source ta|target co|connect in|init cn|count sa|save lo|load".color(Color.BrightGreen).render +
          "\n" + bullet +"table  : br|brief mka mkb mca mcb oa ob l|list ln|lnokey lk|lkey".color(Color.BrightGreen).render +
          "\n" + bullet +"schema : d|def dn|dnokey dk|dkey mkad mkbd mcad mcbd oad obd".color(Color.BrightGreen).render +
          "\n" + bullet +"plan   : p|plan ps|dnokey ps[10] psa psd[n] pa[n] pad[a] padd[n]".color(Color.BrightGreen).render +
          "\n" + bullet +"job    : jn|jnew jl jlr|jld jla|jli jaa jaad js|jstop[name..] jsc jsa jd|jdetail" .color(Color.BrightGreen).render +
          "\n" + bullet +"         jw jcf jfa".color(Color.Green).render

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
            case "ps"           => tuiConf.start_ps(Some(toIntOr(rest.headOption, 10)))
            case "psa"          => tuiConf.start_ps(None)
            case "psd"          => tuiConf.start_ps(Some(toIntOr(rest.headOption, 5)), debug = true)
            case "pa"           => tuiConf.start_pa(Some(toIntOr(rest.headOption, 10)), toMock = false)
            case "paa"          => tuiConf.start_pa(None, toMock = false)
            case "pad"          => tuiConf.start_pa(Some(toIntOr(rest.headOption, 5)), compDebug = true, toMock = false)
            case "padd"         => tuiConf.start_pa(Some(toIntOr(rest.headOption, 5)), compDebug = true, toMock = true)

            case "jn"  | "jnew" => newJob
            case "jl"           => listJob
            case "jlr" | "jld"  => delJob
            case "jla" | "jli"  => insJob

            case "jsa"          => stopAllJob(SM_all)
            case "jsc"          => stopAllJob(SM_activeAll)
            case "js" |"jstop"  => stopJobs(rest)
            case "jd" |"jdetail"=> detailJob
            case "jw"           => show(bullet + "todo".color(Color.Yellow).render + " Job set WHERE clause will be added soon.") // todo
            case "jcf"          =>
              val path = rest.headOption.map(s => "jcf_" + s.trim).getOrElse("jcf_out")
              val full = Paths.get(".").resolve(path)
              val dir = Files.createDirectories(full) // create path
              compareToFileJob(full.toString, 4)

            case "jfv"          => fileToView()
            case "jfa"          => fileToApplyJob(4)

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
