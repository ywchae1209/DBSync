package tui

import org.jline.utils.AttributedString
import tui.SyncTUI.bullet
import tui.layoutzEx.JPromptShell.{RuntimeShellInstance, TUIBarStates}
import tui.layoutzEx._
import zio._

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.Console.println
import scala.collection.convert.ImplicitConversions.`map AsScala`
import scala.collection.mutable

// --------------------------------------------------------------------------------
import tui.TaskStatus._
sealed trait TaskStatus {
  def isDone = this match {
    case _: Done => true
    case _ => false
  }
}
object TaskStatus {
  case object Ready    extends TaskStatus
  case object InProc    extends TaskStatus
  sealed trait Done     extends TaskStatus
    case object Stop  extends Done
    case object Abort   extends Done
    case object Fin extends Done
}

// --------------------------------------------------------------------------------
abstract case class TUITask(name: String) extends HasName{

  private val stateRef = new AtomicReference[TaskStatus](Ready)
  private def setStatus(s: TaskStatus): Unit = stateRef.set(s)
  def status: TaskStatus = stateRef.get()

  def go(cancel: () => Boolean, report0: ReportMsg => Unit)
}

// --------------------------------------------------------------------------------
case class Running( task: TUITask,
                    fiber: Fiber.Runtime[Nothing, Unit],
                    cancelFlag: AtomicBoolean )

// --------------------------------------------------------------------------------
// simple ringBuffer not thread-safe
//case class RingBuffer[A](size: Int = 5) {
//  private val buffer = new Array[A](size)
//  private var idx = 0
//  private var count = 0
//
//  def add(rm: A): Unit = {
//    buffer(idx) = rm
//    idx = (idx + 1) % size
//    if (count < size) count += 1
//  }
//
//  def getAll: Seq[A] = {
//    val start = if (count < size) 0 else idx
//    (0 until count).map(i => buffer((start + i) % size))
//  }
//
//  def getLatest: Option[A] = {
//    if (count == 0) None
//    else {
//      val latestIdx = if (idx == 0) size - 1 else idx - 1
//      Some(buffer(latestIdx))
//    }
//  }
//}

case class JobTUIBarStates(report: ReportMsg => Unit) extends TUIBarStates {

  private val runningState: ConcurrentHashMap[String,ReportMsg] = new ConcurrentHashMap[String, ReportMsg]()

  val reportToMe : ReportMsg => Unit
  = rm => {
    runningState.put(rm.name, rm)
    report(rm)
  }

  override def getStatusMsg(): List[AttributedString]
  = {
    val notDone = runningState.toMap.filterNot(_._2.status.isDone )
    val r = notDone.values.map(rm => AttributedString.fromAnsi(rm.statusString)).toList
    r
  }

  def currentState() = runningState.toMap
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
                       )(implicit rt: Runtime[Any]) {

  // taskQue --> workerLoop --> runningState
  private val runningState: JobTUIBarStates = JobTUIBarStates(report)

  def snapshot: UIO[Snapshot] =
    for {
      s <- state.get
      p <- pendingCount.get
      r <- activeFibers.get.map(_.size)
    } yield Snapshot(s, p, r, runningState.currentState())


  def stateIs(f: RunningTasksState => Boolean, onError: Boolean): Boolean = {
    Unsafe.unsafe { implicit u =>
      rt.unsafe.run(
          state.get.map(f)
        ).toEither
        .fold(
          e => {
            show(bullet + s"fail to check tasks-state. try later. (${e.getMessage})")
            onError
          },
          r =>  r
        )
    }
  }

  def getSnapshot: Option[Snapshot] = {
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
        ( ZIO.succeed(show(bullet + s"all planned tasks finished. use " + "jd".green + " to see result")) *>
          shutdownQueueOnce *>
          state.set(RunningTasksState.Finished) ).unit
      }
    } yield ()
  }

  private def workerLoop: UIO[Unit] = {
    (for {
      task <- taskQue.take
      _ = runningState.reportToMe(ReportMsgTime(task.name, "wait for start.."))
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
        for {
          _ <- ZIO.succeed(r.cancelFlag.set(true)) // *> r.fiber.interrupt
//          _ <- r.fiber.interrupt.delay(10.seconds)
        } yield ()
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
        for {
          _ <- ZIO.succeed(r.cancelFlag.set(true)) //*> r.fiber.interrupt
//          _ <- r.fiber.interrupt.delay(10.seconds)
        } yield()
      }
    } yield ()

  def stopTask(name: String): UIO[Unit] =
    for {
      c <- activeFibers.get
      _ <- ZIO.foreachDiscard(c.filter(_.task.name == name))( r =>
        for {
          _ <- ZIO.succeed(r.cancelFlag.set(true)) // *> r.fiber.interrupt
//          _ <- r.fiber.interrupt.delay(10.seconds)
        } yield ()
      )
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

// --------------------------------------------------------------------------------
trait HasName {
  def name: String
}

// --------------------------------------------------------------------------------
sealed trait StopMode
case object SM_all extends StopMode
case object SM_activeAll extends StopMode
case object SM_select extends StopMode

case class TUIJob[A<:HasName]( show: String => Unit,
                               inst: RuntimeShellInstance, // ,makeTask: A => TUITask
                               name : String = "myJob",
                             )( implicit zioRuntime: Runtime[Any]) {

  private val plans: mutable.Map[String, A] = mutable.Map.empty
  private var currentJob: Option[RunningTasks] = None

  def isRunning: Boolean
  = currentJob.isDefined && currentJob.exists(_.stateIs(_.isRunning, false) )

  def isDone:Boolean
  = currentJob.isDefined && currentJob.exists(_.stateIs(_.isFinished, false ))

  def getSnapshot: Option[Snapshot] = currentJob.flatMap(_.getSnapshot)

  private def start(f: A=> TUITask, threadCount: Int = 2): ZIO[Any, Any, Option[RunningTasks]]
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
          report = _ => ())   // show(s"[$name] $msg") )  // todo ::: <<<<< g3nie
        _ <- ZIO.succeed { currentJob = Some(manager) }
        _ <- ZIO.foreachDiscard(plans.values) { p => manager.offer(f(p)) }
        _ <- manager.finishOffer
        _ <- manager.start(threadCount)
      } yield (currentJob)
  }

  import zio.{Unsafe, _}

  def startAsync(f: A => TUITask, threadCount: Int = 2): Option[RunningTasks] = {
    Unsafe.unsafe { implicit u =>
      zioRuntime.unsafe.run {
        start(f, threadCount)
      }.getOrThrowFiberFailure()
    }
  }

  def stopAsync(mode: StopMode, name: String = ""): Unit = {
    Unsafe.unsafe { implicit u =>
      zioRuntime.unsafe.run {
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

