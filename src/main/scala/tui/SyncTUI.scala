package tui

import org.jline.utils.AttributedString
import schema.DBUtil.{DBConf, createHikariDataSource}
import schema.SchemaCompare.{compareSchemas, fetchSchema, jsonCodec}
import schema.SchemaCompared._
import schema.{ComparePlan, SchemaCompared, TableInfo}
import tui.layoutzEx.InputPrompt._
import tui.layoutzEx.JPromptShell._
import tui.layoutzEx._
import utils.LogHelper.{extractBetween, getFileList}
import zio.json.{DecoderOps, EncoderOps}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.ConcurrentHashMap
import javax.sql.DataSource
import scala.Console.println
import scala.collection.convert.ImplicitConversions.`map AsScala`
import scala.collection.mutable
import scala.util.Try

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
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future

  // --------------------------------------------------------------------------------
  import zio._

  import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

  sealed trait TaskStatus {
    def isDone = this match {
      case done: Done => true
      case _ => false
    }
  }
  case object Ready    extends TaskStatus
  case object InProgress    extends TaskStatus
  case object Started  extends TaskStatus
  sealed trait Done     extends TaskStatus
  case object Stopped  extends Done
  case object Failed   extends Done
  case object Finished extends Done


  case class ReportMsg(name: String, msg: String, state: TaskStatus = Ready) {
    def statusString = f"${name.take(32)}%-32s $state%-10s $msg"
  }

  abstract case class TUITask(name: String) extends HasName{

    private val stateRef = new AtomicReference[TaskStatus](Ready)
    private def setStatus(s: TaskStatus): Unit = stateRef.set(s)
    def status: TaskStatus = stateRef.get()

    def go(cancel: () => Boolean, report0: ReportMsg => Unit)
    def go0( cancel: () => Boolean, report0: ReportMsg => Unit ): Unit = {
      def report(n: String, m: String, s: TaskStatus) = report0(ReportMsg(n, m, s))
      setStatus(Started)
      report(name, "started", Started)
      try {
        var i = 0
        while (i < 600) {
          if (cancel()) {
            setStatus(Stopped)
            report(name, "stopped", Stopped)
            return
          }
          Thread.sleep(100)
          report(name, s"progress=$i", InProgress)
          i += 1
        }
        setStatus(Finished)
        report(name, "finished", Finished)

      } catch {
        case _: InterruptedException => setStatus(Stopped); report(name, "interrupted", Stopped) ; return
        case e: Throwable            => setStatus(Failed); report(name, s"failed : ${e.getMessage}", Failed); throw e
      }
    }
  }

  case class Running( task: TUITask,
                      fiber: Fiber.Runtime[Nothing, Unit],
                      cancelFlag: AtomicBoolean )

  case class JobTUIBarStates(report: ReportMsg => Unit) extends TUIBarStates {

    private val runningState: ConcurrentHashMap[String,ReportMsg] = new ConcurrentHashMap[String, ReportMsg]()
    val reportToMe : ReportMsg => Unit
    = rm => {
      runningState.put(rm.name, rm)
      report(rm)
    }

    override def getStatusMsg(): List[AttributedString]
    = {
      val notDone = runningState.toMap.filterNot(_._2.state.isDone )
      val r = notDone.values.map(rm => AttributedString.fromAnsi(rm.statusString)).toList
      r
    }

    def currentState() = runningState.toMap
    def showCurrentState() = currentState().foreach(println)
  }

  // --------------------------------------------------------------------------------
  sealed trait RunningTasksState {
    def isRunning: Boolean = this match {
      case RunningTasksState.Running | RunningTasksState.Draining => true
      case _ => false
    }
    def isFinished: Boolean = this match {
      case RunningTasksState.Finished => true
      case _ => false
    }
  }
  object RunningTasksState {
    case object BeforeStart extends RunningTasksState
    case object Running     extends RunningTasksState
    case object Draining    extends RunningTasksState
    case object Finished    extends RunningTasksState
  }

  // --------------------------------------------------------------------------------
  case class Snapshot( state: RunningTasksState,
                       pending: Int,
                       running: Int,
                       reports: Map[String, ReportMsg] ) {}

  // --------------------------------------------------------------------------------
  case class RunningTasks(
                           name: String,
                           taskQue: Queue[TUITask],
                           activeFibers: Ref[Set[Running]],
                           pendingCount: Ref[Int],        // queued + running
                           accepting: Ref[Boolean],       // allow offer
                           queueClosed: Ref[Boolean],     // prevent duplicated shutdown
                           state: Ref[RunningTasksState],
                           show: String => Unit,
                           report: ReportMsg => Unit
                         ) {

    // taskQue --> workerLoop --> runningState
    private val runningState: JobTUIBarStates = JobTUIBarStates(report)

    def snapshot: UIO[Snapshot] =
      for {
        s <- state.get
        p <- pendingCount.get
        r <- activeFibers.get.map(_.size)
      } yield Snapshot(s, p, r, runningState.currentState())


    def stateIs(f: RunningTasksState => Boolean, onError: Boolean)(implicit rt: Runtime[Any]): Boolean = {
      Unsafe.unsafe { implicit u =>
        rt.unsafe.run(
            state.get.map(f)
          ).toEither
          .fold(
            e => {
              show(bullet + s"fail to check tasks-state. try later. ($e.getMessage})")
              onError
            },
            r =>  r
          )
      }
    }

    def getSnapshot(implicit rt: Runtime[Any]): Option[Snapshot] = {
      Unsafe.unsafe{ implicit u =>
        rt.unsafe.run(snapshot).toEither.fold(
          e => {
            show(bullet + s"fail : get snapshot of running-tasks : ${e.getMessage}")
            None
          },
          r => Some(r)
        )
      }
    }

    private def shutdownQueueOnce: UIO[Unit] = {
      queueClosed.modify { closed =>
        if (closed) (false, true)
        else        (true, false)
      }.flatMap ( shouldClose =>
        if(shouldClose) taskQue.shutdown else ZIO.unit )
    }

    private def onTaskFinished: UIO[Unit] = {
      for {
        left   <- pendingCount.updateAndGet(_ - 1)
        accept <- accepting.get
        _      <- ZIO.when(left == 0 && !accept) {
          ( ZIO.succeed(show(bullet + s"all planned tasks finished. use " + "jd".color(Color.Green).render + " to see result")) *>
            shutdownQueueOnce *>
            state.set(RunningTasksState.Finished) ).unit
        }
      } yield ()
    }

    private def workerLoop: UIO[Unit] = {
      (for {
        task <- taskQue.take
        _ = runningState.reportToMe(ReportMsg(task.name, "wait for start.."))
        cancelFlag = new AtomicBoolean(false)
        effect = ZIO.attemptBlockingInterrupt {
          val cancel = () => cancelFlag.get()
          task.go(cancel, runningState.reportToMe)
        }.catchAll(_ => ZIO.unit)
        fiber <- effect.fork
        running = Running(task, fiber, cancelFlag)
        _ <- activeFibers.update(_ + running)
        _ <- fiber.await.ensuring(
          activeFibers.update(_ - running) *> onTaskFinished
        )
      } yield ()).forever
    }

    def start(parallelism: Int): UIO[Unit] = {
      for {
        _ <- state.set(RunningTasksState.Running)
        _ <- ZIO.foreachDiscard(1 to parallelism)(_ => workerLoop.forkDaemon)
      } yield ()
    }

    def stopAllTasks: UIO[Unit] =
      for {
        _ <- ZIO.succeed(show( bullet + "stopped all & waiting tasks."))
        _ <- accepting.set(false)
        _ <- shutdownQueueOnce
        _ <- state.set(RunningTasksState.Finished)
        c <- activeFibers.getAndSet(Set.empty)
        _ <- ZIO.foreachParDiscard(c){ r =>
          ZIO.succeed(r.cancelFlag.set(true)) *> r.fiber.interrupt
        }
      } yield ()

    def drainAndShutdown: UIO[Unit] =
      for {
        _    <- ZIO.succeed(show(bullet + s"stop accepting new tasks. drain current tasks [$name]"))
        _    <- state.set(RunningTasksState.Draining)
        _    <- accepting.set(false)
        left <- pendingCount.get
        _    <- if(left != 0) ZIO.unit else {
          (
            (ZIO.succeed(show(bullet + s"no pending tasks. [$name]")) *>
              shutdownQueueOnce *>
              state.set(RunningTasksState.Finished))
          ).as(())
        }
      } yield ()

    def clearQueue: UIO[Unit] = taskQue.takeAll.unit

    def stopAllActiveTasks: UIO[Unit] =
      for {
        _ <- ZIO.succeed(show(bullet + "stopped current active tasks."))
        c <- activeFibers.getAndSet(Set.empty)
        _ <- ZIO.foreachParDiscard(c){ r =>
          ZIO.succeed(r.cancelFlag.set(true)) *> r.fiber.interrupt
        }
      } yield ()

    def stopTask(name: String): UIO[Unit] =
      for {
        c <- activeFibers.get
        _ <- ZIO.foreachDiscard(c.filter(_.task.name == name))( r =>
          ZIO.succeed(r.cancelFlag.set(true)) *> r.fiber.interrupt )
      } yield ()

    def offer(task: TUITask): UIO[Boolean] =
      accepting.get.flatMap {
        case false => ZIO.succeed(false)
        case true => taskQue.offer(task).flatMap {
          case true  => pendingCount.update(_ + 1).as(true)
          case false => ZIO.succeed(false)
        }
      }

    def finishOffer: UIO[Unit] = accepting.set(false)


    def getRunningState: JobTUIBarStates = runningState

    def currentState = runningState.currentState()
  }

  trait HasName {
    def name: String
  }

  sealed trait StopMode
  case object SM_all extends StopMode
  case object SM_activeAll extends StopMode
  case object SM_select extends StopMode

  case class TUIJob[A<:HasName]( show: String => Unit,
                                 inst: RuntimeShellInstance // ,makeTask: A => TUITask
                               ) {

    var name: String = "TUIJob-name"
    private val plans: mutable.Map[String, A] = mutable.Map.empty
    private var currentJob: Option[RunningTasks] = None

    def isRunning(implicit rt: Runtime[Any]):Boolean
    = currentJob.isDefined && currentJob.exists(_.stateIs(_.isRunning, false) )

    def isDone(implicit rt: Runtime[Any]):Boolean
    = currentJob.isDefined && currentJob.exists(_.stateIs(_.isFinished, false ))

    def getSnapshot(implicit rt: Runtime[Any]): Option[Snapshot] = currentJob.flatMap(_.getSnapshot)

    private def start(f: A=> TUITask,threadCount: Int = 2): ZIO[Any, Any, Option[RunningTasks]]
    = currentJob match {
      case Some(_) =>
        ZIO.succeed {
          show(bullet + "Already started or done. make new job.")
          currentJob
        }
      case None =>
        for {
          queue  <- Queue.unbounded[TUITask]
          _       = show(bullet + s"start with $threadCount threads")
          active <- Ref.make(Set.empty[Running])
          count  <- Ref.make(0)
          allow  <- Ref.make(true)
          closed <- Ref.make(false)
          state  <- Ref.make[RunningTasksState](RunningTasksState.BeforeStart)
          manager = RunningTasks(
            name = name,
            taskQue = queue,
            activeFibers = active,
            pendingCount = count,
            accepting = allow,
            queueClosed = closed,
            show = show,
            state = state,
            report = _ => ())   // show(s"[$name] $msg") )
          _ <- ZIO.succeed { currentJob = Some(manager) }
          _ <- ZIO.foreachDiscard(plans.values) { p => manager.offer(f(p)) }
          _ <- manager.finishOffer
          _ <- manager.start(threadCount)
        } yield (currentJob)
    }

    import zio.{Runtime, Unsafe, _}

    def startAsync(f: A => TUITask, runtime: Runtime[Any], threadCount: Int = 2): Option[RunningTasks] = {
      Unsafe.unsafe { implicit u =>
        runtime.unsafe.run {
          start(f, threadCount)
        }.getOrThrowFiberFailure()
      }
    }

    def stopAsync(runtime: Runtime[Any], mode: StopMode, name: String = ""): Unit = {
      Unsafe.unsafe { implicit u =>
        runtime.unsafe.run {
          stop(mode, name)
        }.getOrThrowFiberFailure()
      }
    }

    private def stop(mode: StopMode, name: String = ""): UIO[Unit] = currentJob match {
      case None => ZIO.succeed(show(bullet + "no-active task exist."))
      case Some(ts) =>
        mode match {
          case SM_all             => ts.stopAllTasks
          case SM_activeAll       => ts.stopAllActiveTasks
          case _ if name.nonEmpty => ts.stopTask(name) *> ZIO.succeed(show(bullet + s" task $name stopped."))
          case _                  => ZIO.succeed(show(bullet + "need task-name"))
        }
    }

    def submit(plan: A, f: A => TUITask): UIO[Unit] = currentJob match {
      case None    => ZIO.succeed(add(plan))
      case Some(m) =>
        val t = f(plan)
        m.offer(t) *> ZIO.succeed(show(bullet + s"plan(${plan.name}) is added "))
    }

    // -------------------------------
    def removeIf(f: String => Boolean) = {
      val es = plans.filter{case (k,_) => f(k)}
      for((k,v) <- es){
        plans -= k
      }
    }

    def add(task: A): Unit = plans.put(task.name, task)
    def list(): Map[String, A] = plans.toMap
    def clearPlan(): Unit = {
      plans.clear()
      currentJob = None
    }
  }

  case class TUIConfState(show: String => Unit,
                          inst: RuntimeShellInstance) {

    private val dafaultConf1 = DBConf(
      url = "jdbc:oracle:thin:@arkdata.iptime.org:1523/XE",
      user = "CDCTEST",
      schemaName = "CDCTEST",
      pass = "CDCTEST")

    private val defaultConf2 = DBConf(
      url = "jdbc:oracle:thin:@arkdata.iptime.org:1522/ORCLPDB1",
      user = "CDCTEST",
      schemaName = "CDCTEST",
      pass = "CDCTEST")

    private var planConf: String = "plan_temp.conf"
    private var dbconf1: Option[DBConf] = Some(dafaultConf1)
    private var dbconf2: Option[DBConf] = Some(defaultConf2)
    private var dataSource1: Option[DataSource] = None
    private var dataSource2: Option[DataSource] = None
    private var Compared: Option[SchemaCompared] = None     // save -- load
    // --------------------------------------------------------------------------------

    implicit val ioterm: Option[Terminal] = Some(inst.term)
    implicit val iterm: Terminal = inst.term

    def getDataSource1 = dataSource1
    def getDataSource2 = dataSource2

    private def notLoaded
    = show("configuration not loaded. use: " + "init | load".color(Color.Green).render)

    def updateConSetting(kind: String, isA: Boolean) {

      val c = if(isA) dbconf1 else dbconf2
      val conf = readLineSeq(inst.term, Seq(
          (s"DB connect setting ($kind)",
               " url     ? ".color(Color.Green).render, c.map(_.url)) -> false,
          ("", " id      ? ".color(Color.Green).render, c.map(_.user)) -> false,
          ("", " schema  ? ".color(Color.Green).render, c.map(_.user)) -> false,
          ("", " pwd     ? ".color(Color.Green).render, c.map(_.pass)) -> true,
        ))

      show( layout( "",
        s"-- your setting for $kind ---",
        table(
          Seq( "url", "id", "schema","pwd"),
          Seq( Seq( conf(0), conf(1), conf(2), conf(3).map(_ => '*') ) ) ) ).render )

      val confirm = askConfirm(inst.term)
      if(!confirm) return

      if(isA) { dbconf1 = Some(DBConf(conf(0), conf(1), conf(2), conf(3))) }
      else { dbconf2 = Some(DBConf(conf(0), conf(1), conf(2), conf(3))) }

      show( s"DB connect setting($kind) is updated.")
    }

    private def emptyConf = {
      val empty = dbconf1.isEmpty || dbconf2.isEmpty
      if (empty) show("source target first..")
      empty
    }
    def emptyDataSource = {
      val empty = dataSource1.isEmpty || dataSource2.isEmpty
      if (empty) show("check connect first.")
      empty
    }

    def emptyCompared = {
      val empty = Compared.isEmpty
      if (empty) notLoaded
      empty
    }

    private def isEmpty[A](l: Seq[A]): Boolean = {
      val empty = l.isEmpty
      if (empty) show("empty..")
      empty
    }

    // todo :: when invalid setting input...
    def connect() {
      if (emptyConf) return

      val js = JobSpinner.jobSpinner[Option[String]]("connect".color(Color.Cyan).render)
      Future {
        if (dataSource1.isEmpty) {
          js.setMessage("STEP 1 connection pool for source ")
          dataSource1 = Some(createHikariDataSource(dbconf1.get))
        }
        if (dataSource2.isEmpty) {
          js.setMessage("STEP 2 connection pool for target ")
          dataSource2 = Some(createHikariDataSource(dbconf2.get))
        }
        js.setFinished(None)
      }
      js.run(clearOnStart= false, clearOnExit = false, terminal= ioterm)
    }

    def init(): Either[RuntimeError, SchemaCompared] = {
      if ( emptyConf || emptyDataSource) return Left(InputError("check connect first."))

      val js = JobSpinner.jobSpinner[SchemaCompared]("init".color(Color.Cyan).render)
      Future {
        val ds1 = dataSource1.get
        val ds2 = dataSource2.get

        val schema1 = fetchSchema(ds1, dbconf1.get.schemaName, s => js.setMessage("STEP 1 " + s))
        val schema2 = fetchSchema(ds2, dbconf2.get.schemaName, s => js.setMessage("STEP 2 " + s))
        val out = compareSchemas(schema1, schema2, s => js.setMessage("STEP 3 " + s))
        js.setFinished( out)
      }

      js.run(clearOnStart= false, clearOnExit = false, terminal= ioterm)
        .flatMap( _.getOr.toRight(InputError("empty")))
    }

    private def detail(l: List[TableInfo]) {
      if(l.isEmpty) { show("not exist") } else {
        val ts = selectTables(l)
        ts.foreach( t => show(tableDetail(t)) )
      }
    }

    def save() {

      if(emptyCompared) return

      val pname = readNotEmpty( inst.term, "", "? plan name(use as legal filename) ? ".color(Color.Red).render, Some("wow"), false)
      val fpath = s"plan_$pname.conf"

      Try {
        val p = Paths.get(fpath)
        if (Files.exists(p)) throw new IllegalArgumentException(s"$fpath already exist.")
        val j = Compared.get.toJsonPretty
        val b = j.getBytes(StandardCharsets.UTF_8)
        Files.write(p, b)
        planConf = fpath
        show(s"your plan is written to $fpath")
        show(s"current plan name is $pname")
      }.toEither
        .left.foreach(e => show(s"failed to save($fpath) ".color(Color.Red).render + e.toString) )
    }

    def load() {

      val list = getFileList("plan_", ".conf").fold(
        e => { show(s"fail to locate plan conf: ${e.toString}"); List.empty},
        r => r
      )

      val pname = SingleBox
        .singleBox("select plan", list)
        .run( clearOnStart= false, clearOnExit= false, terminal= Some(inst.term))
        .fold(
          e => {show(s"fail to select : ${e.toString}"); None},
          r => extractBetween(r.selectedItem, "plan_", ".conf"))

      if (pname.isEmpty) return

      Try {
        val fpath = s"plan_${pname.get}.conf"
        val p = Paths.get(fpath)
        val b = Files.readAllBytes(p)
        val s = new String(b, StandardCharsets.UTF_8)
        val o = s.fromJson[SchemaCompared]
        if(o.isLeft) throw new IllegalStateException(s"cannot load conf. $fpath")
        planConf = fpath
        Compared = o.toOption
        show(s"plan conf is loaded. : $fpath")
        show(s"current plan name is ${pname.get}")
      }
    }

    // -------
    def selectPlansNotIn(names: Set[String]): Seq[ComparePlan] = {
      if(emptyCompared) return Seq.empty
      val o = Compared.get
      val ts = selectTables(o.comparable.filterNot(c => names.contains(c.name)))
      o.comparePlans.filter(p => ts.exists( _.name == p.table.name) )
    }

    def selectPlans(): Seq[ComparePlan] = {
      if(emptyCompared) return Seq.empty
      val o = Compared.get
      val ts = selectTables(o.comparable)
      o.comparePlans.filter(p => ts.exists( _.name == p.table.name) )
    }

    // ------
    def selectPlans0(o: SchemaCompared): Seq[ComparePlan] = {
      val ts = selectTables(o.comparable)
      o.comparePlans.filter(p => ts.exists( _.name == p.table.name) )
    }

    private def showPlan(o: SchemaCompared) = {
      val selected = selectPlans0(o)
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

    def updateCount() {

      if( emptyCompared || emptyDataSource) return
      val sc = Compared.get
      val cs = sc.comparable
      if(isEmpty(cs)) return

      val selected = selectTables(cs)
      val js = JobSpinner.jobSpinner[Seq[TableInfo]](s"fetch count of ${selected.size} tables".color(Color.Cyan).render)

      Future{
        js.setMessage("start")
        val update = selected.map{ s =>
          js.setMessage(s"${s.name}")
          s.fetchCount(dataSource1.get, dataSource2.get)
        }
        js.setMessage("done")

        val copied = cs.map(c => update.find(s => s.name == c.name).getOrElse(c))
        Compared = Compared.map( _.copy( comparable = copied))
        js.setFinished(update)
      }

      js.run(clearOnStart= false, clearOnExit = false, terminal= ioterm)
        .map(_.getOr.getOrElse(Seq.empty))
        .foreach( a => show(tableOfInfos(a)) )
    }

    private def withCompared( f: SchemaCompared => Unit): Unit = {
      if(!emptyCompared)
        f(Compared.get)
    }

    def show_b     = withCompared(o => show(summary(o)))
    def show_mka   = withCompared(o => show(tableOfInfos(o.mismatchKey.map(_._1))))
    def show_mkb   = withCompared(o => show(tableOfInfos(o.mismatchKey.map(_._2))))
    def show_mca   = withCompared(o => show(tableOfInfos(o.mismatchCols.map(_._1))))
    def show_mcb   = withCompared(o => show(tableOfInfos(o.mismatchCols.map(_._2))))
    def show_oa    = withCompared(o => show(tableOfInfos(o.onlyInDb1)))
    def show_ob    = withCompared(o => show(tableOfInfos(o.onlyInDb2)))
    def show_l     = withCompared(o => show(tableOfInfos(o.comparable)))
    def show_ln    = withCompared(o => show(tableOfInfos(o.filterNoKey)))
    def show_lk    = withCompared(o => show(tableOfInfos(o.filterKey)))
    def show_mkad  = withCompared(o => detail(o.mismatchKey.map(_._1)))
    def show_mkbd  = withCompared(o => detail(o.mismatchKey.map(_._2)))
    def show_mcad  = withCompared(o => detail(o.mismatchCols.map(_._1)))
    def show_mcbd  = withCompared(o => detail(o.mismatchCols.map(_._2)))
    def show_oad   = withCompared(o => detail(o.onlyInDb1))
    def show_obd   = withCompared(o => detail(o.onlyInDb2))
    def show_d     = withCompared(o => detail(o.comparable))
    def show_dn    = withCompared(o => detail(o.filterNoKey))
    def show_dk    = withCompared(o => detail(o.filterKey))
    def show_plan  = withCompared(o => showPlan(o))

    // compare --------------------------------
    def start_ps(n: Option[Int], debug: Boolean = false) {
      if( (!emptyCompared) && (!emptyDataSource)) {
        val o = Compared.get
        val selected = selectPlans0(o)
        if(isEmpty(selected)) return

        for( p <- selected ) {
          show( rowElements(p.table).map(_.render).mkString(" "))
          p.goCompare(
            s1 = dataSource1.get,
            s2 = dataSource2.get,
            limit = n,
            cancel = () => false,
            notice = _ => (),
            verbose = true,
            compDebug = debug)
        }
      }
    }

    // compare & apply ------------------------
    def start_pa(n: Option[Int], compDebug: Boolean = false, applDebug: Boolean = false) {
      if( (!emptyCompared) && (!emptyDataSource)) {
        val o = Compared.get
        val selected = selectPlans0(o)
        if (isEmpty(selected)) return

        for (p <- selected) {
          show(rowElements(p.table).map(_.render).mkString(" "))
          p.goCompareApply(
            s1 = dataSource1.get,
            s2 = dataSource2.get,
            limit = n,
            cancel = () => false,
            notice = _ => (),
            verbose = true,
            compDebug = compDebug,
            applDebug = applDebug)
        }
      }
    }

    // init -----------------------------------
    def start_init() {
      for( o <- init()) {
        show(summary(o))
        Compared = Some(o)
      }
    }
  }

  def main(args: Array[String]): Unit = {

    val terminal = JLineTerminalWrapper.create().toOption
    if(terminal.isEmpty)
      println("Terminal Driver not found.")

    JPromptShell.promptShell(terminal.get, "DBSync/")(
      new JPromptShell.ShellHandler{

        private var tuiConfState : TUIConfState = null
        private var tuiJobState : Option[TUIJob[ComparePlan]] = None

        def tuiConf(implicit show: String => Unit, inst: RuntimeShellInstance): TUIConfState = {
          if(tuiConfState == null) tuiConfState = TUIConfState(show, inst)
          tuiConfState
        }


        def showJobRunning(implicit show: String => Unit) = show(bullet + "make job list first.")
        def showJobNotRunning(implicit show: String => Unit) = show(bullet + "No job is running.")

        def makeJob()(implicit show: String => Unit, inst: RuntimeShellInstance): TUIJob[ComparePlan] = {
          show(bullet + "Job List : select tables for job")
          val ps = tuiConf.selectPlans()
          val out = TUIJob[ComparePlan](show, inst) //, p => TUITask(p.name)) // todo ::
          for (p <- ps) { out.add(p) }
          show(bullet + s"Job List : created with ${ps.size} tables.")
          out
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
              if(j.isRunning) { showJobRunning } else {
                val confirm = askConfirm(inst.term, bullet + "Job list(not-running) will be cleared\n")
                if(confirm) tuiJobState = Some( makeJob())
              }
            case None =>
              tuiJobState = Some( makeJob())
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
                  case SM_all       => bullet +"stop " + "all & waiting".color(Color.Green).render + " tasks.\n"
                  case SM_activeAll => bullet +"stop " + "current active".color(Color.Green).render + " tasks.\n"
                  case SM_select    => bullet +"command error. please report to developer.\n"
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

        def compareToApplyJob(f: ComparePlan => TUITask, par: Int)(implicit show: String => Unit, inst: RuntimeShellInstance, rt: Runtime[Any]): Unit = {
          withJob{ j =>
            if(tuiConfState.emptyDataSource) return

            val sz = j.list().size
            val confirm = askConfirm(inst.term, bullet + s"Job start($sz tables) with $par threads.\n")
            if(confirm) {
              if(par > 4) { show( bullet + s"$par is too many. i'll use 4 threads.(DBMS may overburden)") }
              val n = par.min(4)

              val st = j.startAsync(f, rt, n)
              if(st.isDefined) {
                inst.setBatStates(st.map(_.getRunningState))
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
          "\n" + bullet +"so|source ta|target co|connect in|init cn|count sa|save lo|load".color(Color.BrightGreen).render +
          "\n" + bullet +"br|brief mka mkb mca mcb oa ob l|list ln|lnokey lk|lkey".color(Color.BrightGreen).render +
          "\n" + bullet +"d|def dn|dnokey dk|dkey mkad mkbd mcad mcbd oad obd".color(Color.BrightGreen).render +
          "\n" + bullet +"p|plan ps|dnokey ps[10] psa psd[n] pa[n] pad[a] padd[n]".color(Color.BrightGreen).render +
          "\n" + bullet +"jn|jnew jl jlr|jld jla|jli jaa jaad js|jstop[name..] jsc jsa jd|jdetail" .color(Color.BrightGreen).render +
          "\n" + bullet +"jw jcf jfa".color(Color.Green).render

        def help(show: String => Unit) = show(helpStr)

        def toIntOr(s: Option[String], orElse: Int) = s.flatMap(_.toIntOption).getOrElse(orElse)

        def commandHandler(cmd: String, rest: List[String])(implicit show: String => Unit, inst: RuntimeShellInstance)
        : Option[String] = {

          implicit val zioRuntime: Runtime[Any] = Runtime.default

          cmd  match {
            case "so"| "source" => tuiConf.updateConSetting("source", isA = true)
            case "ta"| "target" => tuiConf.updateConSetting("target", isA = false)
            case "co"| "connect"=> tuiConf.connect()
            case "in"| "init"   => tuiConf.start_init()
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
            case "pa"           => tuiConf.start_pa(Some(toIntOr(rest.headOption, 10)))
            case "paa"          => tuiConf.start_pa(None)
            case "pad"          => tuiConf.start_pa(Some(toIntOr(rest.headOption, 5)), compDebug = true)
            case "padd"         => tuiConf.start_pa(Some(toIntOr(rest.headOption, 5)), compDebug = true, applDebug = true)

            // job command
            case "jn"  | "jnew" => newJob
            case "jl"           => listJob
            case "jlr" | "jld"  => delJob
            case "jla" | "jli" => insJob

            // job stop
            case "jsa"          => stopAllJob(SM_all)
            case "jsc"          => stopAllJob(SM_activeAll)
            case "js" |"jstop"  => stopJobs(rest)
            case "jd" |"jdetail"=> detailJob
            case "jw"           => show(bullet + "todo".color(Color.Yellow).render + " Job set WHERE clause will be added soon." ) // todo
            case "jcf"          => show(bullet + "todo".color(Color.Yellow).render + " Job Compare to File will be added soon." ) // todo
            case "jfa"          => show(bullet + "todo".color(Color.Yellow).render + " Job File to Apply will be added soon." ) // todo

            case "jaa"          => compareToApplyJob(_.toCompareApplyTask(
              tuiConf.getDataSource1.get, tuiConf.getDataSource2.get, false
            ), toIntOr(rest.headOption, 2))

            case "jaad"          => compareToApplyJob(_.toCompareApplyTask(
              tuiConf.getDataSource1.get, tuiConf.getDataSource2.get, true
            ), toIntOr(rest.headOption, 2))

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
