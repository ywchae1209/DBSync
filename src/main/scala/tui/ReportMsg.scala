package tui

import layoutzEx.StringWithColor
import schema.ComparePlan.jobLogger
import tui.TaskStatus._

import java.time.{Instant, ZoneId}

trait ReportMsg {
  val name: String
  val nameStr = f"${name.take(30)}%-30s"
  val status: TaskStatus
  def statusString: String
}

case class ReportMsgStop(name: String, statusString: String) extends ReportMsg{
  val status = Stop
}

case class ReportMsgAbort(name: String, statusString: String) extends ReportMsg{
  val status = Abort
}

case class ReportMsgTime(name: String, desc: String) extends ReportMsg
{
  val status = InProc
  val now = System.currentTimeMillis()
  val time = Instant.ofEpochMilli(now).atZone(ZoneId.systemDefault()).toLocalDateTime().toString
  def statusString = s"${nameStr.yellow} $time : $desc"

  def intervalStr(msg: String) = {
    val m = System.currentTimeMillis()
    val mStr = Instant.ofEpochMilli(m).atZone(ZoneId.systemDefault()).toLocalDateTime().toString
    time + "--" + mStr + msg
  }
}

case class ReportMsgElapse(name: String, since: ReportMsgTime, desc: String) extends ReportMsg
{
  val status = InProc
  val now = System.currentTimeMillis()
  val startTime = since.time
  val elapse = (now - since.now) / 1000.0
  val elapseStr = f"$elapse%.3f".red

  def statusString = s"${nameStr.yellow} $elapseStr sec. since ${since.time} : $desc"
}

// --------------------------------------------------------------------------------
case class ReportMsgIt(name: String, r1:Long, r2:Long, s:Long, u:Long, a:Long, b:Long, m: String, status: TaskStatus) extends ReportMsg
{
  val msg = {
    if (status.isDone) s"comp: $m ra:$r1 rb:$r2 ".yellow + s"sa:$s ".cyan + s"up:$u oa:$a ob:$b".green
    else               s"comp: $m ra:$r1 rb:$r2 sa:$s up:$u oa:$a ob:$b"
  }
  def statusString = f"$nameStr $status%-8s $msg"
}

// --------------------------------------------------------------------------------
case class ReportMsgAp(name: String,
                       ic: Long, uc: Long, dc: Long, sc: Long,
                       si: Long, su: Long, sd: Long, status: TaskStatus) extends ReportMsg
{
  val msg = {
    if(status.isDone) (s"appl: sa:$sc ".yellow + s"in:$ic up:$uc de:$dc ".green + s"si:$si su:$su sd:$su")
    else              (s"appl: sa:$sc in:$ic up:$uc de:$dc si:$si su:$su sd:$su")
  }
  def statusString = f"$nameStr $status%-8s $msg"
}

case class ReportMsgApCancel(name: String, checkPoint: Int) extends ReportMsg
{
  val status = Stop
  def statusString = f"$nameStr $status%-8s Stop checkpoint(${checkPoint.toString.green})"
}

case class ReportMsgApSkip(name: String, offset: Int, m: String) extends ReportMsg
{
  val status = InProc
  def statusString = f"${name.take(32)}%-32s $status%-8s skipped(${offset.toString.green}) $m"
}

object ReportMsg {
  def noticeWithLog(notice: ReportMsg => Unit)(rm: ReportMsg): Unit = {
    notice(rm)
    jobLogger.info(rm.statusString)
  }

}
