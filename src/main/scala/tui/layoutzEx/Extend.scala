package tui.layoutzEx

import org.jline.utils.AttributedString
import tui.SyncTUI.bullet

import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.collection.concurrent.TrieMap
import scala.util.Try

trait withViewAll{
  def viewAll: Element
}

// ================================================================================
object JPromptShell {

  import org.jline.utils.Status

  sealed trait TaskStatus {
    override def toString() = this match {
      case Wait => "Wait"
      case Run =>  "Run"
      case fin: Fin => fin match {
        case Done => "Done"
        case Stop => "Stop"
      }
    }
  }

  case object Wait extends TaskStatus
  case object Run  extends TaskStatus
  sealed trait Fin extends TaskStatus
  case object Done extends Fin
  case object Stop extends Fin

  case class TaskInfo( id: Int, name: String,
                       taskStatus: TaskStatus = Wait,
                       element: Element = "") {

    @volatile var tick : Int = 0

    def render: String = {
      tick += 1
      rowTight(
        f" $id%3d ",
        f" $name%-10s ",
        f" $taskStatus%-10s ",
        spinner(frame = tick),
        " ",
        element
      ).render
    }
  }

  trait TUIBarStates {
    def getStatusMsg(): List[AttributedString]
  }

  trait RuntimeShellInstance {

    // --------------------------------------------------------------------------------
    @volatile var showStatus: Boolean = false
    private val lockS = new Object()
    private var barHeight: Int = 5

    @volatile var barStates: Option[TUIBarStates] = None
    private val lockT = new Object()

    def showStatusBar() = lockS.synchronized { showStatus = true}
    def hideStatusBar() = lockS.synchronized { showStatus = false}
    def needShowStatusBar() = lockS.synchronized { showStatus }

    def setStatusHeight(h: Int) = { barHeight = h }

    def setBarStates(st: Option[TUIBarStates])= lockT.synchronized { barStates = st }
    def getBarStatesMsg = barStates.map( _.getStatusMsg()).getOrElse(List.empty)

    // --------------------------------------------------------------------------------

    val term : Terminal
    def runShell(): Unit
  }

  trait ShellHandler {

    def onStart(term: Terminal): Unit
    def onExit(term: Terminal): Unit
    def handleIO( prompt: String, line: String, self: RuntimeShellInstance): Unit
    def stopIOLoop() : Boolean
  }

  def promptShell[A](jTermWrapper: JLineTerminalWrapper,
                     prompt: String,
                     shellApp: ShellHandler,
                     semaphore: ScreenSemaphore)
  = new RuntimeShellInstance {

      val term  = jTermWrapper
      val jterm = jTermWrapper.jTerm
      private val jStateBar = Status.getStatus(jterm, true)

      @volatile private var shouldContinue = true

      override def runShell(): Unit = {

        semaphore.strictly{ () =>
          jterm.puts(org.jline.utils.InfoCmp.Capability.clear_screen)
          jterm.puts(org.jline.utils.InfoCmp.Capability.cursor_home)

          shellApp.onStart(jTermWrapper)
          jterm.flush
        }

        val ioThread = new Thread( () => runIOLoop(this), "JShellInput")
        ioThread.setDaemon(true)
        ioThread.start()

        val renderThread = new Thread( () => runRenderStateBarLoop(), "JShellRenderStateBar")
        renderThread.setDaemon(true)
        renderThread.start()

        try while(shouldContinue) Thread.sleep(50)
        catch  { case _: InterruptedException => ()}
        finally {
          shellApp.onExit(jTermWrapper)
        }
      }

      def show(s: String): Unit = {
        semaphore.strictly{ () =>
          jTermWrapper.writeLine(s)
          jTermWrapper.flush()
        }
      }

      private def runIOLoop(self: RuntimeShellInstance) = {
        while(shouldContinue) {
          val p1 = for {
            l <- Try(InputPrompt.readLine(jTermWrapper, prompt)(semaphore))
            p <- Try(shellApp.handleIO(prompt, l, self))
          } yield p

          if(p1.isFailure) {
            // todo :: g3nie -- logging
            shouldContinue = false
          } else {
            if(shellApp.stopIOLoop())
              shouldContinue = false
          }
          Thread.sleep(50)
        }
      }

      def renderStateBar(): Boolean = {
        try{
          if( needShowStatusBar()) {
            val ls = getBarStatesMsg
            if(ls.isEmpty) jStateBar.hide() else {
              semaphore.loosely { () =>
                jStateBar.setBorder(true)
                jStateBar.update( ls.asJava)
              }
            }
          } else {
            jStateBar.hide()
          }
          true
        }
        catch { case e: Throwable =>
          semaphore.loosely { () =>
            jStateBar.update( e.getMessage.split("\\n").map( AttributedString.fromAnsi).toList.asJava )
          }
          false
        }
      }

      private def runRenderStateBarLoop(): Unit = {
        while(shouldContinue) {
          val continue = renderStateBar()
          if(continue)
            Thread.sleep(300)
          else
            return
        }
      }
    }
}

// ================================================================================
object InputPrompt {

  def askConfirm(term: Terminal, msg: String = "")(implicit semaphore: ScreenSemaphore): Boolean
  = {
    val confirm = readNotEmpty(term, "", msg + " is ok (yes/no) ? ".color(Color.Yellow).render, None, false)
    confirm.toLowerCase.startsWith("y")
  }

  def readNotEmpty(term: Terminal, ask: String, prompt: String, pre: Option[String], isSecret: Boolean)(implicit semaphore: ScreenSemaphore): String = {
    var out = ""
    do {
      if(ask != "")
        term.writeLine(ask)
      out = readLine(term, prompt, pre, isSecret)
    } while(out == "")

    out
  }

  def readLineSeq(term: Terminal, asks: Seq[((String, String, Option[String]), Boolean)])(implicit semaphore: ScreenSemaphore) = {
    asks.map{ case ((ask, prompt, pre), isSecret) => readNotEmpty(term, ask, prompt, pre, isSecret)}
  }

  def readForStopOr(term: Terminal)
  : Boolean = {
    while(true) {
      val input = term.readInput()
      val key = KeyParser.parse(input, term)
      key match {
        case Key.Char('s') | Key.Char('S') |
             Key.Char('q') | Key.Char('Q') | Key.Escape => return true
        case Key.Char(_)                   => return false
        case _ =>
      }
    }
    false
  }

  def readLine(term: Terminal,
               prompt: String,
               pre: Option[String] = None,
               isSecret: Boolean = false)(implicit semaphore: ScreenSemaphore): String = {

    val quitKey = Key.Ctrl('Q')
    val moveLeft ="\b"

    def go(prompt0: String = "prompt> "): String = {

      @volatile var shouldContinue = true
      @volatile var done: Boolean = false
      @volatile var cursor: Int = 0
      val buf = new StringBuilder(pre.getOrElse(""))

      // 화면에 표시할 문자를 결정하는 헬퍼 함수
      def mask(s: String): String = if (isSecret) "*" * s.length else s
      def maskChar(c: Char): String = if (isSecret) "*" else c.toString

      def redrawFromCursor(): Unit = {
        val remaining = buf.substring(cursor)
        term.write(mask(remaining) + " ")
        (0 to remaining.length).foreach(_ => term.write("\b"))
        term.flush()
      }

      semaphore.strictly{ () =>
        term.write(prompt0)
        redrawFromCursor()
        term.flush
      }

      while( shouldContinue && !done) {
        val input = term.readInput()
        val key = KeyParser.parse(input, term)

        key match {
          case `quitKey`     =>
            shouldContinue = false

          case Key.Char(c)   =>
            buf.insert(cursor, c)
            semaphore.strictly{ () =>
              term.write(maskChar(c)) // 입력 문자 마스킹
              cursor += 1
              redrawFromCursor()
            }

          case Key.Enter     =>
            semaphore.strictly { () =>
              term.writeLine("")
            }
            done = true

          case Key.Left | Key.Ctrl('B') if(cursor > 0) =>
            cursor -= 1
            semaphore.strictly { () =>
              term.write(moveLeft)
              term.flush()
            }

          case Key.Right | Key.Ctrl('F') if cursor < buf.length =>
            semaphore.strictly{ () =>
              term.write(maskChar(buf(cursor))) // 오른쪽 이동 시 글자 마스킹 출력
              term.flush()
            }
            cursor += 1

          case Key.Delete | Key.Ctrl('D') if cursor < buf.length =>
            buf.deleteCharAt(cursor)
            semaphore.strictly { () =>
              redrawFromCursor()
            }

          case Key.Backspace | Key.Ctrl('H') if cursor > 0 =>
            buf.deleteCharAt(cursor - 1)
            cursor -= 1
            semaphore.strictly{  () =>
              term.write(moveLeft)
              redrawFromCursor()
            }

          case Key.Home | Key.Ctrl('A')     =>
            semaphore.strictly { () =>
              (0 until cursor).foreach(_ => term.write(moveLeft))
              term.flush()
            }
            cursor = 0

          case Key.End  | Key.Ctrl('E') =>
            semaphore.strictly { () =>
              val remaining = buf.substring(cursor)
              term.write(mask(remaining)) // 끝으로 이동 시 남은 글자 마스킹 출력
              term.flush()
            }
            cursor = buf.length

          case Key.Ctrl('L') =>
            semaphore.strictly{ () =>
              term.clearScreen()
              term.write(prompt0 + mask(buf.toString())) // 화면 갱신 시 전체 마스킹
              (0 until (buf.length - cursor)).foreach(_ => term.write(moveLeft))
              term.flush()
            }

          case Key.Ctrl('K') =>
            if (cursor < buf.length) {
              val lenToRemove = buf.length - cursor
              buf.delete(cursor, buf.length)
              semaphore.strictly { () =>
                term.write(" " * lenToRemove)
                (0 until lenToRemove).foreach(_ => term.write(moveLeft))
                term.flush()
              }
            }

          case Key.Ctrl('U') if cursor > 0 =>
            val lenToRemove = cursor
            buf.delete(0, cursor)
            semaphore.strictly { () =>
              (0 until lenToRemove).foreach(_ => term.write(moveLeft))
              term.write(" " * lenToRemove)
              (0 until lenToRemove).foreach(_ => term.write(moveLeft))
              cursor = 0
              redrawFromCursor()
            }

          case Key.Ctrl('W') if cursor > 0 =>
            val textBefore = buf.substring(0, cursor)
            val lastWordPattern = "(\\s*\\S+\\s*)$".r
            lastWordPattern.findFirstMatchIn(textBefore).foreach { m =>
              val start = m.start
              val len = cursor - start
              buf.delete(start, cursor)
              semaphore.strictly { () =>
                (0 until len).foreach(_ => term.write(moveLeft))
                term.write(" " * len)
                (0 until len).foreach(_ => term.write(moveLeft))
                cursor = start
                redrawFromCursor()
              }
            }
          case _             =>
        }
      }
      val out= buf.toString()
      buf.clear()
      out
    }

    go(prompt)
  }
}

// ================================================================================
object JobSpinner {

  val bullet = "> ".color(Color.Yellow).render

  trait withJobCallback[A]{
    def setFinished(a: A): Unit
    def setMessage(s: String): Unit
  }

  case class JSState[A](tick: Int, getOr: Option[A] = None)
  sealed trait JSMsg
  case object JSTick extends JSMsg

  def jobSpinner[A](name: String): LayoutzApp[JSState[A], JSMsg] with withJobCallback[A] =
    new LayoutzApp[JSState[A], JSMsg] with withJobCallback[A] {

      @volatile var desc: String = name
      @volatile var done: Option[A] = None

      override def setFinished(a: A) = { done = Option(a) }
      override def setMessage(s: String): Unit = { desc = s }

      override def init: (JSState[A], Cmd[JSMsg]) = (JSState(0), Cmd.none)

      override def update(msg: JSMsg, state: JSState[A]): (JSState[A], Cmd[JSMsg]) =
        msg match {
          case JSTick =>
            if(done.isDefined) {
              state.copy(100, done) -> (if(state.tick == 100) Cmd.exit else Cmd.none)
            } else {
              state.copy( (state.tick + 1) % 100) -> Cmd.none
            }
        }

      override def subscriptions(state: JSState[A]): Sub[JSMsg] = Sub.time.everyMs(32, JSTick)

      def progressBar(tick: Int) = {
        val width = 50
        val filled = if(done.isDefined) 50 else (tick / 2)
        val bar = (0 until width).map { i =>
          if (i >= filled) { "░".color(Color.BrightBlack) } else {
            val ratio = i.toDouble / width
            "█".color(Color.True((ratio * 180).toInt + 50, ((1 - ratio) * 200).toInt + 55, 255))
          }
        }
        rowTight(bar: _*)
      }

      override def view(state: JSState[A]): Element = {
        if(done.isEmpty)
          row (
            progressBar(state.tick),
            spinner(s"$name $desc", state.tick/3)
          )
        else
          rowTight( "> ".color(Color.Yellow), Text("finished "), Text(name).color(Color.Green) )
      }
    }
}

// ================================================================================
object SingleBox {

  case class SBState( opts: Seq[String],
                      selected: Int = 0,
                      currentPg: Int = 0,
                      cursor: Int = 0,
                      active: Boolean = false )
  {
    def selectedItem = opts(selected)
    override def toString: String =
      s"SingleBox( #opt: ${opts.size}, selected: $selectedItem)"
  }

  sealed trait SBMsg
  private case object SBUp     extends SBMsg
  private case object SBDown   extends SBMsg
  private case object SBPgUp   extends SBMsg
  private case object SBPgDown extends SBMsg
  private case object SBSelect extends SBMsg
  private case object SBOK     extends SBMsg

  def singleBox(header: String,
                options: Seq[String],
                selection: Int = 0,
                pageRow: Int = 20,
                active: Boolean = true)
  = new LayoutzApp[SBState, SBMsg] with withViewAll {

    val totalRow = options.size
    val unitRows = pageRow.min(totalRow)
    val totalPage = if( totalRow % pageRow == 0) (totalRow / pageRow) else (totalRow / pageRow) + 1
    val zippedOpts = options.zipWithIndex

    override def init: (SBState, Cmd[SBMsg]) =
      SBState( options, selection, active = active) -> Cmd.none

    override def update(msg: SBMsg, state: SBState): (SBState, Cmd[SBMsg]) = {
      msg match {
        case SBUp     =>
          val c = (state.cursor + totalRow - 1) % totalRow
          val p = (c / unitRows)
          state.copy( currentPg = p, cursor = c ) -> Cmd.none

        case SBDown   =>
          val c = (state.cursor + 1) % totalRow
          val p = (c / unitRows)
          state.copy( currentPg = p, cursor = c ) -> Cmd.none

        case SBPgUp   =>
          val p = (state.currentPg + unitRows - 1) % totalPage
          val c = p * pageRow
          state.copy( currentPg = p, cursor = c) -> Cmd.none

        case SBPgDown =>
          val p = (state.currentPg + 1) % totalPage
          val c = p * unitRows
          state.copy( currentPg = p, cursor = c) -> Cmd.none

        case SBOK     =>
          state.copy( active = false) -> Cmd.exit

        case SBSelect  =>
          if(!state.active) state -> Cmd.none else {
            state.copy( selected = state.cursor)  -> Cmd.none
          }
      }
    }

    override def subscriptions(state: SBState): Sub[SBMsg] = Sub.onKeyPress {
      case Key.Enter    => Some(SBOK)
      case Key.Up       => Some(SBUp)
      case Key.Down     => Some(SBDown)
      case Key.PageUp   => Some(SBPgUp)
      case Key.PageDown => Some(SBPgDown)
      case Key.Char(c) if c == 'n' => Some(SBPgDown)
      case Key.Char(c) if c == 'p' => Some(SBPgUp)
      case Key.Char(c) if c == 'j' => Some(SBDown)
      case Key.Char(c) if c == 'k' => Some(SBUp)
      case Key.Char(c) if c == 's' => Some(SBSelect)
      case Key.Char(c) if c == ' ' => Some(SBSelect)
      case _ => None
    }

    def viewport(state: SBState, rowPerPage: Int = unitRows) = {
      val from = state.currentPg * rowPerPage
      val to = from + rowPerPage
      zippedOpts.slice( from ,to)
    }

    def toElement(state: SBState, rowPerPage: Int = unitRows) = {
      val hd = {
        val pg = rowTight(
          f"  ${state.currentPg + 1}%2d",
          f"/${totalPage}%-3d".color(Color.BrightBlack)
        )
        val se = rowTight(
          "  ✓ ",
          s"${state.selectedItem}".color(Color.Green),
        )

        rowTight(
          if (active) s"    $header  ".color(Color.Yellow)
          else        s"    $header  ",
          if(totalPage == 1) "" else pg,
          se
        )
      }

      val l0 = s"      │"
      val ls = {
        val out = viewport(state).map{ case (opt, idx) =>

          val selected = idx == state.selected
          val ma: Element = if(selected) "● ".color(Color.BrightCyan) else "○ "
          val se: Element = if(selected) opt.color(Color.BrightCyan).style(Style.Underline) else opt

          val ln = if(totalPage > 1) f"${idx+1}%5d │" else l0
          val lb = ln.color(Color.BrightBlack)

          val highlight =
            if (state.active && idx == state.cursor)
              rowTight( ln, "►", ma, opt.color(Color.BrightCyan) ).bg(Color.BrightBlue)
            else
              rowTight( lb, " ", ma, se)
          highlight
        }
        out ++ Seq.fill(rowPerPage - out.length)(l0.color(Color.BrightBlack))
      }

      layout( (hd +: ls):_*)
    }

    override def viewAll: Element = {
      toElement(init._1.copy(cursor = -1, active = false), totalRow)
    }

    override def view(state: SBState): Element = {
      toElement(state)

    }
  }
}

// ================================================================================
object MultiBox {

  case class MBState( opts: Seq[String],
                      selected: Set[Int] = Set.empty,
                      currentPg: Int = 0,
                      cursor: Int = 0,
                      active: Boolean = false ) {
    override def toString: String =
      s"MultiBox( #opt: ${opts.size}, #selected: ${selected.size}): "+ selected.mkString(",")
  }

  sealed trait MBMsg
  private case object MBUp     extends MBMsg
  private case object MBDown   extends MBMsg
  private case object MBPgUp   extends MBMsg
  private case object MBPgDown extends MBMsg
  private case object MBToggle extends MBMsg
  private case object MBAllOn  extends MBMsg
  private case object MBAllOff extends MBMsg
  private case object MBOK     extends MBMsg

  def multiBox(header: String,
               options: Seq[String],
               selection: Set[Int] = Set.empty,
               pageRow: Int = 20,
               truncateLimit: Int = 64,
               active: Boolean = false ) =
    new LayoutzApp[MBState, MBMsg] with withViewAll {

      val el : Element= ""
      // realLength(el)

      val totalRow = options.size
      val unitRows = pageRow.min(totalRow)
      val totalPage = if( totalRow % pageRow == 0) (totalRow / pageRow) else (totalRow / pageRow) + 1

      val allSelection = (0 until totalRow).toSet
      val zippedOpts = options.zipWithIndex

      override def init: (MBState, Cmd[MBMsg]) =
        MBState( options, selection, active = active ) -> Cmd.none

      override def update(msg: MBMsg, state: MBState): (MBState, Cmd[MBMsg]) =
        msg match {
          case MBUp     =>
            val c = (state.cursor + totalRow - 1) % totalRow
            val p = (c / unitRows)
            state.copy( currentPg = p, cursor = c ) -> Cmd.none

          case MBDown   =>
            val c = (state.cursor + 1) % totalRow
            val p = (c / unitRows)
            state.copy( currentPg = p, cursor = c ) -> Cmd.none

          case MBPgUp   =>
            val p = (state.currentPg + unitRows - 1) % totalPage
            val c = p * pageRow
            state.copy( currentPg = p, cursor = c) -> Cmd.none

          case MBPgDown =>
            val p = (state.currentPg + 1) % totalPage
            val c = p * unitRows
            state.copy( currentPg = p, cursor = c) -> Cmd.none

          case MBOK  =>
            if (!active || state.selected.nonEmpty) {
              state.copy(active = false) -> Cmd.exit
            } else {
              state -> Cmd.none
            }

          case MBToggle  =>
            if (!state.active) state -> Cmd.none else {
              val i = state.cursor
              val s = if( state.selected.contains(i) ) (state.selected- i) else (state.selected + i)
              state.copy( selected = s) -> Cmd.none
            }

          case MBAllOn =>
            if (!state.active) state -> Cmd.none else {
              state.copy(selected = allSelection) -> Cmd.none
            }

          case MBAllOff  =>
            if (!state.active) state -> Cmd.none else {
              state.copy(selected = Set.empty) -> Cmd.none
            }
        }

      override def subscriptions(state: MBState): Sub[MBMsg] = Sub.onKeyPress {
        case Key.Enter    => Some(MBOK)
        case Key.Up       => Some(MBUp)
        case Key.Down     => Some(MBDown)
        case Key.PageUp   => Some(MBPgUp)
        case Key.PageDown => Some(MBPgDown)
        case Key.Char(c) if c == 'n' => Some(MBPgDown)
        case Key.Char(c) if c == 'p' => Some(MBPgUp)
        case Key.Char(c) if c == 'j' => Some(MBDown)
        case Key.Char(c) if c == 'k' => Some(MBUp)
        case Key.Char(c) if c == 's' =>  Some(MBToggle)
        case Key.Char(c) if c == ' ' =>  Some(MBToggle)
        case Key.Char(c) if c == 'a' => Some(MBAllOn)
        case Key.Char(c) if c == 'A' => Some(MBAllOff)
        case _ => None
      }

      def viewport(state: MBState, rowPerPage: Int = unitRows) = {
        val from = state.currentPg * rowPerPage
        val to = from + rowPerPage
        zippedOpts.slice( from ,to)
      }

      def toElement(state: MBState, rowPerPage: Int = unitRows): Element = {
        val hd = {
          val pg = rowTight(
            f"  ${state.currentPg + 1}%2d",
            f"/${totalPage}%-3d".color(Color.BrightBlack)
          )
          val i0 = s"  must select at least one".color(Color.BrightBlack)

          val se = rowTight(
            "  ✓",
            f"${state.selected.size}%4d".color(Color.Green),
            f":${totalRow}%-4d".color(Color.BrightBlack)
          )

          rowTight(
            if (active) s"    $header  ".color(Color.Yellow)
            else        s"    $header  ",
            if(totalPage == 1) "" else pg,
            if(state.selected.isEmpty ) i0 else se,
          )
        }
        val l0 = "      ┃"
        val ls = {
          val out = {
            viewport(state).map{ case (opt, idx) =>

              val ma: Element =
                if (state.selected.contains(idx))
                  "[●] ".color(Color.BrightCyan)
                else
                  "[ ] "

              val ln = if(totalPage > 1) f"${idx+1}%5d ┃" else l0
              val lb = ln.color(Color.BrightBlack)

              val highlight =
                if (state.active && idx == state.cursor)
                  rowTight( ln, "►", ma, opt ).bg(Color.BrightBlue)
                else
                  rowTight( lb, " ", ma, opt)

              highlight
            }
          }
          out ++ Seq.fill(rowPerPage - out.length)(l0.color(Color.BrightBlack))
        }
        layout( (hd +: ls):_*)
      }

      override def viewAll: Element = {
        toElement( init._1.copy(cursor = -1, active = false ), totalRow )
      }

      override def view(state: MBState): Element = {
        toElement(state)
      }
    }
}

// ================================================================================
object MultiTable {

  case class MTState( selected: Set[Int] = Set.empty,
                      currentPg: Int = 0,
                      cursor: Int = 0,
                      active: Boolean = false ) {

    override def toString: String =
      s"MultiTable( #selected: ${selected.size}): "+ selected.mkString(",")
  }

  sealed trait MTMsg
  private case object MTUp     extends MTMsg
  private case object MTDown   extends MTMsg
  private case object MTPgUp   extends MTMsg
  private case object MTPgDown extends MTMsg
  private case object MTToggle extends MTMsg
  private case object MTAllOn  extends MTMsg
  private case object MTAllOff extends MTMsg
  private case object MTOK     extends MTMsg

  def multiTable( label: String,
                  header: Seq[Element],
                  rows: Seq[Seq[Element]],
                  selection: Set[Int] = Set.empty,
                  pageRow: Int = 20,
                  truncates: Seq[Int] = Seq.empty,  // todo :: trucate
                  active: Boolean = true ) =
    new LayoutzApp[MTState, MTMsg] with withViewAll {

      val totalRow = rows.size
      val unitRows = pageRow.min(totalRow)
      val totalPage = if( totalRow % pageRow == 0) (totalRow / pageRow) else (totalRow / pageRow) + 1
      val allSelection = (0 until totalRow).toSet

      val zippedRows = rows.zipWithIndex

      val prefixedHeader: Seq[Element] = Text("") +: header

      override def init: (MTState, Cmd[MTMsg]) =
        MTState(selection, active = active) -> Cmd.none

      override def update(msg: MTMsg, state: MTState): (MTState, Cmd[MTMsg]) =
        msg match {
          case MTUp     =>
            val c = (state.cursor + totalRow - 1) % totalRow
            val p = (c / unitRows)
            state.copy( currentPg = p, cursor = c ) -> Cmd.none

          case MTDown   =>
            val c = (state.cursor + 1) % totalRow
            val p = (c / unitRows)
            state.copy( currentPg = p, cursor = c ) -> Cmd.none

          case MTPgUp   =>
            val p = (state.currentPg + unitRows - 1) % totalPage
            val c = p * pageRow
            state.copy( currentPg = p, cursor = c) -> Cmd.none

          case MTPgDown =>
            val p = (state.currentPg + 1) % totalPage
            val c = p * unitRows
            state.copy( currentPg = p, cursor = c) -> Cmd.none

          case MTOK  =>
            if (!active || state.selected.nonEmpty) {
              state.copy(active = false) -> Cmd.exit
            } else {
              state -> Cmd.none
            }

          case MTToggle  =>
            if (!state.active) state -> Cmd.none else {
              val i = state.cursor
              val s = if( state.selected.contains(i) ) (state.selected- i) else (state.selected + i)
              state.copy( selected = s) -> Cmd.none
            }

          case MTAllOn =>
            if (!state.active) state -> Cmd.none else {
              state.copy(selected = allSelection) -> Cmd.none
            }

          case MTAllOff  =>
            if (!state.active) state -> Cmd.none else {
              state.copy(selected = Set.empty) -> Cmd.none
            }
        }

      override def subscriptions(state: MTState): Sub[MTMsg] = Sub.onKeyPress{
        case Key.Enter    => Some(MTOK)
        case Key.Up       => Some(MTUp)
        case Key.Down     => Some(MTDown)
        case Key.PageUp   => Some(MTPgUp)
        case Key.PageDown => Some(MTPgDown)
        case Key.Char(c) if c == 'n' => Some(MTPgDown)
        case Key.Char(c) if c == 'p' => Some(MTPgUp)
        case Key.Char(c) if c == 'j' => Some(MTDown)
        case Key.Char(c) if c == 'k' => Some(MTUp)
        case Key.Char(c) if c == 's' =>  Some(MTToggle)
        case Key.Char(c) if c == ' ' =>  Some(MTToggle)
        case Key.Char(c) if c == 'a' => Some(MTAllOn)
        case Key.Char(c) if c == 'A' => Some(MTAllOff)
        case _ => None
      }

      def pageInfo(state: MTState): Element = {
        val pg = rowTight(
          f"${state.currentPg + 1}%2d",
          f"/${totalPage}%-3d".color(Color.BrightBlack)
        )
        if(totalPage == 1) "" else pg
      }

      def selectInfo(state: MTState): Element = {
        val i0 = s"  must select at least one".color(Color.BrightBlack)
        val se = rowTight(
          "  ✓",
          f"${state.selected.size}%4d".color(Color.Green),
          f":${totalRow}%-4d".color(Color.BrightBlack)
        )

        if(state.selected.isEmpty ) i0 else se
      }

      def viewport(state: MTState, rowPerPage: Int = unitRows) = {
        val from = state.currentPg * rowPerPage
        val to = from + rowPerPage
        zippedRows.slice( from ,to)
      }

      def toElement(state: MTState, rowPerPage: Int = unitRows) = {

        val hd: Element = {
          val lb:Element =
            if (active) s"    $label  ".color(Color.Yellow)
            else        s"    $label  ".color(Color.BrightBlack)

          rowTight(lb, pageInfo(state), selectInfo(state))
        }

        val rows = viewport(state, rowPerPage).map{ case (es, idx) =>
          val isActive = idx == state.cursor
          val isSel = state.selected.contains(idx)
          val mark = {
            if(isActive && isSel) f"${idx+1}%4d*".color(Color.BrightGreen).style(Style.Bold ++ Style.Reverse)
            else if(isActive)     f"${idx+1}%4d ".color(Color.BrightCyan).style(Style.Bold ++ Style.Reverse)
            else if(isSel)        f"${idx+1}%4d*".color(Color.BrightCyan)
            else                  f"${idx+1}%4d ".color(Color.BrightBlack)
          }

          mark +: (es.map { cell =>
            if (isActive && isSel) cell.style(Style.Bold ++ Style.Reverse).color(Color.BrightGreen)
            else if (isActive)     cell.style(Style.Bold ++ Style.Reverse).color(Color.BrightCyan)
            else if (isSel)        cell.style(Style.Bold ++ Style.Underline)
            else                   cell
          })
        }

        val filled = rows ++ Seq.fill( rowPerPage - rows.length)(Seq(Text("")) )

        layout(
          hd,
          table(
            prefixedHeader,
            filled
          )
        )
      }

      override def viewAll: Element = {
        toElement(init._1.copy(cursor = -1, active = false), totalRow)
      }

      override def view(state: MTState): Element = {
        toElement(state)
      }
    }
}

// ================================================================================