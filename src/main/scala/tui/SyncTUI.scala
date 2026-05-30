package tui

import schema.DBUtil.{DBConf, createHikariDataSource}
import schema.SchemaCompare.{checkGrant, compareSchemas, fetchSchema, jsonCodec}
import schema.SchemaCompared._
import schema.{SchemaCompared, TableInfo}
import tui.layoutzEx.InputPrompt._
import tui.layoutzEx.JPromptShell._
import tui.layoutzEx._
import utils.LogHelper.maybe
import zio.json.{DecoderOps, EncoderOps}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import javax.sql.DataSource
import scala.util.Random

object SyncTUI {

  implicit val sema: ScreenSemaphore = ScreenSemaphore()

  val bannerElement = {
    val m = ">".color(Color.Yellow)
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
        rowTight(m," project : ", "DeltaFlow".color(Color.Green) ," project"),
        rowTight(m," mission : ", "compare tables & synchronize"),
        rowTight(m," author  : ", "DeltaFlow team"),
        "",
        ""
      )
    )
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Test codes

  def demo_bg(name: String, count: Int) = {
    val nums = (1 until count)
    val ti = nums.map(i => {
      TaskInfo( i, s"bg_name_$i", element = "some information".color(Color.BrightBlack))
    } )

    val tasks = {
      val ts = Tasks(name)
      ti.foreach(i => ts.putIfAbsent(i.id, i))
      ts
    }

    val works =
      nums.map{ n =>
        new Thread( () => {
          Thread.sleep(1000 * n)
          for (p <- 0 to 100 by 5) {
            tasks.updateTaskWith(n)( _.map( t => {
              t.copy( taskStatus = Run ,
                element = rowTight( f"progress: $p%3d..", ("▀" * (p/2)).color(Color.BrightBlack)) )
            } ) )
            Thread.sleep(100 + Random.nextInt(1000))
          }
          tasks.updateTaskWith(n)( _.map( t => {
            t.copy( taskStatus = Done ,element = "progress: finished")
          } ) )
        } )
      }

    tasks -> works
  }

  def main(args: Array[String]): Unit = {

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future

    val terminal = JLineTerminalWrapper.create().toOption
    if(terminal.isEmpty)
      println("Terminal Driver not found.")

    JPromptShell.promptShell(terminal.get, "DBSync/")(
      new JPromptShell.ShellHandler{

        // main conf
        // ----------------------------------------------
        val confPath = "conf.json"
        var dbconf1: Option[DBConf] = None
        var dbconf2: Option[DBConf] = None
        var dataSource1: Option[DataSource] = None
        var dataSource2: Option[DataSource] = None
        var schemaCompared: Option[SchemaCompared] = None
        // ----------------------------------------------

        @volatile var exit: Boolean = false

        override def onStart(term: Terminal): Unit = {
          term.enterRawMode()
          bannerElement.render.split("\\n").foreach { term.writeLine }
        }
        override def onExit(term: Terminal): Unit = {
          term.exitRawMode()
        }

        def inputConf(kind: String, show: String => Unit, inst: RuntimeShellInstance) = {
          val conf = {
            readLineSeq(inst.term, Seq(
              s"conf/$kind"  -> "? url ? ".color(Color.Green).render -> false,
              ""  -> "? id  ? ".color(Color.Green).render -> false,
              ""  -> "? pwd ? ".color(Color.Green).render -> true,
            ))
          }
          val cs: Seq[Seq[Element]] = Seq( Seq( conf(0), conf(1), conf(2).map(_ => '*')) )

          val tbl = layout( kind.color(Color.Red), table(Seq( "url", "id", "pwd"), cs ) )
          show( tbl.render)

          val isOk = readNotEmpty(inst.term, "is this " + "ok?".color(Color.Red).render, "y/n ?", false)

          if(isOk.toLowerCase.startsWith("y"))
            Some(conf)
          else
            None
        }

        def connect(show: String => Unit, inst: RuntimeShellInstance) = {
          if (dbconf1.isEmpty || dbconf2.isEmpty) {
            show("source target first..")
            Left(InputError(""))
          }

          val js = JobSpinner.jobSpinner[Option[String]]("loading".color(Color.Cyan).render)
          Future {
            dbconf1 = Some( DBConf(
              url = "jdbc:oracle:thin:@arkdata.iptime.org:1523/XE",
//              url = "jdbc:oracle:thin:@arkdata.iptime.org:1521/ORCLPDB1",
              user = "CDCTEST",
              pass = "CDCTEST"))
            dbconf2 = Some( DBConf(
//              url = "jdbc:oracle:thin:@arkdata.iptime.org:1524/XE",
              url = "jdbc:oracle:thin:@arkdata.iptime.org:1522/ORCLPDB1",
              user = "CDCTEST",
              pass = "CDCTEST") )

            if (dataSource1.isEmpty) {
              js.setMessage("1. create connection pool for source ")
              dataSource1 = Some(createHikariDataSource(dbconf1.get))
            }
            if (dataSource2.isEmpty) {
              js.setMessage("2. create connection pool for target ")
              dataSource2 = Some(createHikariDataSource(dbconf2.get))
            }
            js.setFinished(None)
          }
          js.run(clearOnStart= false, clearOnExit = false, terminal= Some(inst.term))
        }

        def initConf(show: String => Unit,inst: RuntimeShellInstance) = {
          if ( dbconf1.isEmpty || dbconf2.isEmpty || dataSource1.isEmpty || dataSource2.isEmpty) {
            show("check connect first..")
            Left(InputError(""))
          } else {
            val js = JobSpinner.jobSpinner[SchemaCompared]("loading".color(Color.Cyan).render)
            Future {
              val ret = for{
                ds1 <- dataSource1
                ds2 <- dataSource2
              } yield {
                val con1 = ds1.getConnection
                val con2 = ds2.getConnection
                val schema1 = {
                  js.setMessage("1. fetch schema from source")
                  fetchSchema(ds1, dbconf1.map(_.user).get)
                }
                val schema2 = {
                  js.setMessage("2. fetch schema from target")
                  fetchSchema(ds2, dbconf2.map(_.user).get)
                }
                val cs = {
                  js.setMessage("3. compare and analyze schemas...")
                  compareSchemas(schema1, schema2)
                }
                val out = {
                  js.setMessage("4. check privileges to LOB hash")
                  checkGrant(ds1, ds2, cs)
                }

                con1.close()
                con2.close()
                out
              }
              js.setFinished(ret.orNull)
            }
            js.run(clearOnStart= false, clearOnExit = false, terminal= Some(inst.term))
          }
        }

        def detail(l: List[TableInfo], show: String => Unit, inst: RuntimeShellInstance): Unit = {
          if(l.isEmpty) {
            show("not exist")
            return
          }

          val ts = selectTableInfos(l, inst)
          ts.foreach( t => show(tableDetail(t)) )
        }

        def saveConf(o: SchemaCompared, show: String => Unit) = {
          val out = maybe("--saveConf--"){
            val j = o.toJsonPretty
            val p = Paths.get(confPath)
            val b = j.getBytes(StandardCharsets.UTF_8)
            show(j)
            Files.write(p, b)
          }
          if(out.isEmpty)
            show("save conf failed.".color(Color.Red).render)
        }

        def loadConf(show: String => Unit) = {
          val out = maybe("--loadConf--") {
            val p = Paths.get(confPath)
            val b = Files.readAllBytes(p)
            val s = new String(b, StandardCharsets.UTF_8)
            val o = s.fromJson[SchemaCompared]
            schemaCompared = o.toOption
            show(o.fold(l => "load failed: conf.json", r => "conf.json is loaded."))
          }
          if(out.isEmpty)
            show("load conf failed.".color(Color.Red).render)
        }

        def selectTableInfos(l: List[TableInfo], inst: RuntimeShellInstance): Seq[TableInfo] = {
          if(l.isEmpty)
            return List.empty

          val (hdr, rows) = headerAndRows(l)
          val ms = MultiTable
            .multiTable("select tables",  hdr, rows)
            .run( clearOnStart=false, clearOnExit= false, terminal= Some(inst.term))
          val ss = ms.toOption.map(s => s.selected).getOrElse(Set.empty)
          val ts = l.zipWithIndex.flatMap{ case (a, i) => if(ss.contains(i)) Some(a) else None }
          ts
        }

        def selectPlans(o: SchemaCompared, inst: RuntimeShellInstance) = {
          val ts = selectTableInfos(o.comparable, inst)
          val selected = ts.flatMap(t => o.comparePlans.find( _.table.name == t.name))
          selected
        }

        def showPlan(o: SchemaCompared, show: String => Unit, inst: RuntimeShellInstance) = {
          val selected = selectPlans(o, inst)
          selected.foreach(p => {
            show("--------------------------------------------------")
            show( rowElements(p.table).map(_.render).mkString(" "))
            show("--------------------------------------------------")
            show("1. sql: select".color(Color.Red).render)
            show("   " + p.sourceSql)
            show("2. sql: insert to target".color(Color.Red).render)
            show("   " + p.insertSql)
            show("3. sql: update to target".color(Color.Red).render)
            show("   " + p.updateSql)
            show("4. sql: delete from target".color(Color.Red).render)
            show("   " + p.deleteSql)
            show("")
          })
        }

        def startPlan(o: SchemaCompared, show: String => Unit, inst: RuntimeShellInstance,
                      count: Option[Int], compDebug: Boolean = false, applyTarget: Boolean = false, applDebug: Boolean = false): Unit = {

          val selected = selectPlans(o, inst)
          if(selected.isEmpty) {
            show("empty")
            return
          }

          selected.foreach(p => {
            show( rowElements(p.table).map(_.render).mkString(" "))
            p.goWith(dataSource1.get, dataSource2.get, count, compDebug, applyTarget, applDebug)
          } )
        }

        def updateCount(show: String => Unit, inst: RuntimeShellInstance): Unit = {

          if( schemaCompared.isEmpty || dataSource1.isEmpty || dataSource2.isEmpty) {
            show( "init first.")
            return
          }
          val o: SchemaCompared = schemaCompared.get
          val l = o.comparable
          if(l.isEmpty) { show("empty"); return }

          val (hdr, rows) = headerAndRows(l)
          val selected = MultiTable.multiTable("select tables",  hdr, rows)
            .run( clearOnStart=false, clearOnExit= false, terminal= Some(inst.term))
          val lst = selected.toOption.map(s => s.selected).getOrElse(Set.empty)
          val js = JobSpinner.jobSpinner[List[TableInfo]](s"fetch row-count of ${lst.size} tables ".color(Color.Cyan).render)

          Future{
            js.setMessage("start")
            val updated =
              l.zipWithIndex.map{ case (a, i) =>
                if(lst.contains(i)) {
                  js.setMessage(s"${a.name}")
                  a.fetchCount(dataSource1.get, dataSource2.get)
                } else
                  a
              }
            js.setMessage("done")
            schemaCompared = Some(o.copy(comparable = updated))
            js.setFinished(updated)
          }
          val result = js.run(clearOnStart= false, clearOnExit = false, terminal= Some(inst.term))
          result.map(_.getOr.getOrElse(List.empty))
            .foreach(tableOfInfos)
        }

        def confCommand(cmd: String, show: String => Unit, inst: RuntimeShellInstance): Option[String] = {

          def notLoaded = show("configuration not loaded. use: " + "init | load".color(Color.Green).render)
          def hashOn = show("if table contains LOB/CLOB/NCLOB, i'll compare it with hash.".color(Color.Red).render )
          def hashOff = show("i'll compare LOB/CLOB/NCLOB compare by contents")

          cmd  match {
            case "so"| "source" => inputConf("source", show, inst)    // todo
            case "ta"| "target" => inputConf("target", show, inst)    // todo
            case "co"| "connect"=> connect(show, inst)
            case "cn"| "count"  => updateCount(show, inst)
            case "sa"| "save"   => schemaCompared.map(o => saveConf(o, show)).getOrElse(notLoaded)  // todo
            case "lo"| "load"   => loadConf(show)   // todo
            case "b" | "brief"  => schemaCompared.map(o => show(summary(o))).getOrElse(notLoaded)
            case "mka"          => schemaCompared.map(o => show(tableOfInfos(o.mismatchKey.map(_._1)))).getOrElse(notLoaded)
            case "mkb"          => schemaCompared.map(o => show(tableOfInfos(o.mismatchKey.map(_._2)))).getOrElse(notLoaded)
            case "mca"          => schemaCompared.map(o => show(tableOfInfos(o.mismatchCols.map(_._1)))).getOrElse(notLoaded)
            case "mcb"          => schemaCompared.map(o => show(tableOfInfos(o.mismatchCols.map(_._2)))).getOrElse(notLoaded)
            case "oa"           => schemaCompared.map(o => show(tableOfInfos(o.onlyInDb1))).getOrElse(notLoaded)
            case "ob"           => schemaCompared.map(o => show(tableOfInfos(o.onlyInDb2))).getOrElse(notLoaded)
            case "mkad"         => schemaCompared.map(o => detail(o.mismatchKey.map(_._1), show, inst)).getOrElse(notLoaded)
            case "mkbd"         => schemaCompared.map(o => detail(o.mismatchKey.map(_._2), show, inst)).getOrElse(notLoaded)
            case "mcad"         => schemaCompared.map(o => detail(o.mismatchCols.map(_._1), show, inst)).getOrElse(notLoaded)
            case "mcbd"         => schemaCompared.map(o => detail(o.mismatchCols.map(_._2), show, inst)).getOrElse(notLoaded)
            case "oad"          => schemaCompared.map(o => detail(o.onlyInDb1, show, inst)).getOrElse(notLoaded)
            case "obd"          => schemaCompared.map(o => detail(o.onlyInDb2, show, inst)).getOrElse(notLoaded)
            case "l" | "list"   => schemaCompared.map(o => show(tableOfInfos(o.comparable))).getOrElse(notLoaded)
            case "ln"| "lnokey" => schemaCompared.map(o => show(tableOfInfos(o.filterNoKey))).getOrElse(notLoaded)
            case "lk"| "lkey"   => schemaCompared.map(o => show(tableOfInfos(o.filterKey))).getOrElse(notLoaded)
            case "d" | "def"    => schemaCompared.map(o => detail(o.comparable, show, inst)).getOrElse(notLoaded)
            case "dn"| "dnokey" => schemaCompared.map(o => detail(o.filterNoKey, show, inst)).getOrElse(notLoaded)
            case "dk"| "dkey"   => schemaCompared.map(o => detail(o.filterKey, show, inst)).getOrElse(notLoaded)

            case "p" | "plan"   => schemaCompared.map(o => showPlan(o, show, inst)).getOrElse(notLoaded)
            case "ps"| "pstart" => schemaCompared.map(o => startPlan(o, show, inst, Some(10))).getOrElse(notLoaded)
            case "ps100"        => schemaCompared.map(o => startPlan(o, show, inst, Some(100))).getOrElse(notLoaded)
            case "ps1k"         => schemaCompared.map(o => startPlan(o, show, inst, Some(1000))).getOrElse(notLoaded)
            case "ps2k"         => schemaCompared.map(o => startPlan(o, show, inst, Some(2000))).getOrElse(notLoaded)
            case "psa"          => schemaCompared.map(o => startPlan(o, show, inst, None)).getOrElse(notLoaded)
            case "psd"          => schemaCompared.map(o => startPlan(o, show, inst, Some(5), true)).getOrElse(notLoaded)
            case "pad"          => schemaCompared.map(o => startPlan(o, show, inst, None, false, true, true)).getOrElse(notLoaded)
            case "paa"          => schemaCompared.map(o => startPlan(o, show, inst, None, false, true, false)).getOrElse(notLoaded)
            case "hash"         => schemaCompared = schemaCompared.map( _.updatePlan(true)); hashOn
            case "nohash"       => schemaCompared = schemaCompared.map( _.updatePlan(false)); hashOff
            case "cd.."         => return Some("DBSync/")
            case "in"| "init" =>
              val out = initConf(show, inst)
              out.toOption.flatMap(_.getOr).foreach(o => {
                show(summary(o))
                if(!o.crytoGrantedA)
                  show("[NEED GRANT To handle LOB of source]".color(Color.Red).render + "GRANT EXECUTE ON SYS.DBMS_CRYPTO TO user-id;")
                if(!o.crytoGrantedB)
                  show("[NEED GRANT To handle LOB of target]".color(Color.Red).render + "GRANT EXECUTE ON SYS.DBMS_CRYPTO TO user-id;")
                schemaCompared = Some(o)
              })
            case _ => show("[help] " +
              "source target connect init load save brief list ln lk count(cn) def dn dk p ps".color(Color.Green).render)
          }
          Some("DBSync/conf/")
        }

        def jobCommand(cmd: String, show: String => Unit, inst: RuntimeShellInstance) = {

          Some("DBSync/job/")
        }

        override def handleIO(prompt: String, show: String => Unit, line: String, inst: RuntimeShellInstance)
        : Option[String] = {

          import scala.concurrent.ExecutionContext.Implicits.global
          import scala.concurrent.Future

          if(line.isEmpty) return None

          val token = line.trim.split("\\s+")
          val cmd = token(0).toLowerCase

          prompt match {
            case "DBSync/job/"   => return jobCommand(cmd, show, inst)
            case "DBSync/conf/"  => return confCommand(cmd, show, inst)
            case "DBSync/"       =>
              val c1 = token.lift(1).map(_.toLowerCase).getOrElse("")
              cmd match {
                case "c" | "conf" => return confCommand(c1, show, inst)
                case "j" | "job"  =>
                case _ =>
              }

            case "cd" =>
              token.lift(1).map(_.toLowerCase).getOrElse("") match {
                case "conf"   => return Some(s"${prompt}conf")
                case "result" => return Some(s"${prompt}conf")
                case "conf"   => return Some(s"${prompt}conf")
                case _        => return Some(s"${prompt}conf")
              }

            case "s" =>
              token.lift(1).map(_.toLowerCase).getOrElse("") match {
                case "on" =>  inst.showStatusBar()
                case "off" => inst.hideStatusBar()
                case n if n.toIntOption.isDefined =>
                  val a = n.toInt.min(40).max(1)
                  if(!inst.setStatusHeight(a)) show("no-task exist.. run background task first")
                case _ =>
              }

            // j 5 Name
            case "j" =>
              val jobs = token.lift(1).map(_.toLowerCase).getOrElse("") match {
                case n if n.toIntOption.isDefined => n.toInt.min(40).max(5)
                case _ =>  10
              }

              val name = token.lift(2).map(_.toLowerCase).getOrElse("") match {
                case "" => "DEMO-JOBS"
                case a => a
              }

              val (ts, js) = demo_bg(name, jobs)
              show(inst.setTasks(name, ts).toString)
              Future{
                js.foreach{ j =>
                  j.start()
                }
              }
              inst.showStatusBar()
              show(s"background task started: $name")

              return Some("mock-up/jobs > ")

            case "exit" | "q" =>
              show("see you again~~".color(Color.Cyan).render)
              exit = true

            case "" =>
            case other =>
              show("i don't understand : " + other.color(Color.Red).render)
          }
          None
        }
        override def stopIOLoop(): Boolean = exit
      }
    ).runShell()


  }
}
