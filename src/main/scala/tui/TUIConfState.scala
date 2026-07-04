package tui

import schema.DBConf.{HikariDataSourceWithConf, displayTo, displayToWith}
import schema.SchemaCompare.{compareSchemas, fetchSchema, fetchTableNames, jsonCodec}
import schema.SchemaCompared._
import schema.{ComparePlan, DBConf, SchemaCompared, TableInfo}
import tui.SyncTUI.{bullet, tuiLogger}
import tui.TUIConfState.{dafaultConf1, defaultConf2}
import tui.layoutzEx.InputPrompt.{askConfirm, readLineSeq, readNotEmpty}
import tui.layoutzEx.JPromptShell.RuntimeShellInstance
import tui.layoutzEx._
import _root_.table.DiffRow
import utils.LogHelper.getFileList
import zio.json.{DecoderOps, EncoderOps}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import javax.sql.DataSource
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

object TUIConfState {

  val dafaultConf1 = DBConf(
    url = "jdbc:oracle:thin:@192.168.0.78:1521/EE.oracle.docker",
    user = "CDCTEST",
    schema = "CDCTEST",
    pass = "cdctest"
    //    url = "jdbc:oracle:thin:@arkdata.iptime.org:1523/XE",
  )

  val defaultConf2 = DBConf(
    url = "jdbc:oracle:thin:@192.168.0.78:1522/ORA19",
    user = "CDCTEST",
    schema = "CDCTEST",
    pass = "cdctest"
    //      url = "jdbc:oracle:thin:@arkdata.iptime.org:1523/XE",
  )
}

case class TUIConnection( show: String => Unit, inst: RuntimeShellInstance)(implicit sema: ScreenSemaphore){

  implicit val ioterm: Option[Terminal] = Some(inst.term)
  implicit val iterm: Terminal = inst.term

  private var dbconf1: Option[DBConf] = Some(dafaultConf1)
  private var dbconf2: Option[DBConf] = Some(defaultConf2)
  private var dataSource1: Option[HikariDataSourceWithConf] = None
  private var dataSource2: Option[HikariDataSourceWithConf] = None
  private var Compared: Option[SchemaCompared] = None

  private def notLoaded
  = show(bullet + "configuration not loaded. use: " + "init | load".green)

  def connected: Boolean = dbconf1.exists( _.connected) && dbconf2.exists( _.connected)

  val pre_st = "current conf " + "so ta".green
  val pre_cst = "current connection " + "co".green
  val pre_pst = "plan apply with "
  def show_st = displayToWith(pre_st, dbconf1, dbconf2, show )
  def show_cst = displayToWith(pre_cst, dataSource1.map(_.conf), dataSource2.map(_.conf), show )
  def show_pst = displayToWith(pre_pst, Compared.map(_.conf1), Compared.map(_.conf2), show )

  def setWhere(tableName: String, where: String) = {
    Compared.foreach(c => c.setWhere(tableName, where))
  }

  def updateConSetting(kind: String, isA: Boolean) {

    val current = if(isA) dbconf1 else dbconf2
    val in = readLineSeq(inst.term, Seq(
      (s"DB connect setting ($kind)",
        " url     ? ".green, current.map(_.url)) -> false,
      ("", " schema  ? ".green, current.map(_.schema)) -> false,
      ("", " id      ? ".green, current.map(_.user)) -> false,
      ("", " pwd     ? ".green, current.map(_.pass.mkString)) -> true,
    ))

    val conf = DBConf.apply(url= in(0), schema= in(1), user = in(2), pass= in(3))
    conf.display(kind, show)

    val confirm = askConfirm(inst.term)
    if(!confirm) return
    if( conf.neqUrlSchema(current) ) show(bullet + "setting changed. reconnect with " + "co".green)

    if(isA) { dbconf1 = Some(conf) }
    else    { dbconf2 = Some(conf) }

    show( bullet + s"DB connect setting is updated.")
  }

  def confOr(isA :Boolean) = {
    if(isA)
      dbconf1.orElse{show(bullet + "set source first"); None}.filter(_.passIsNotSet("source", show))
    else
      dbconf2.orElse{show(bullet + "set target first"); None}.filter(_.passIsNotSet("target", show))
  }

  def dbConfsOr: Option[(DBConf, DBConf)] = {
    val c1 = confOr(isA = true)
    val c2 = confOr(isA = false)
    for{
      r1 <- c1
      r2 <- c2
    } yield( r1 -> r2)
  }

  def comparedForDataSoure ( conf1: Option[DBConf], conf2: Option[DBConf]): Boolean = {
    Compared.forall( cp => {
      !cp.conf1.neqUrlSchema(conf1) &&
        !cp.conf2.neqUrlSchema(conf2)
    } )
  }

  def comparedDataSourcesOr: Option[(DataSource, DataSource)] = {
    val out = dataSourcesOr

    if(comparedForDataSoure(dataSource1.map(_.conf), dataSource2.map(_.conf)))
      out.orElse{
        show( bullet + "current connection differ from plan's DBConf. check and re-initialize. see" + "so ta co st lst cst".green )
        None
      }
    else
      None
  }
  def dataSourcesOr: Option[(DataSource, DataSource)] = {

    val out = for {
      d1 <- dataSource1.orElse{ show(bullet + "not initialized connection(source) " + "co".green); None}
      d2 <- dataSource2.orElse{ show(bullet + "not initialized connection(source) " + "co".green); None}
    } yield ( d1 -> d2)

    out
  }

  def comparePlanOr = {
    if(Compared.isEmpty) notLoaded
    Compared
  }

  private def isEmpty[A](l: Seq[A]): Boolean = {
    val empty = l.isEmpty
    if (empty) show(bullet + "empty")
    empty
  }

  def connect() {
    val confs = dbConfsOr
    if (confs.isEmpty) return
    val (conf1, conf2) = confs.get

    if( conf1.alreadyInitalized("source", show) || conf2.alreadyInitalized("target", show) ) {
      val confirm = askConfirm(inst.term, bullet + "re-initialize connection pool after close")
      if(!confirm)
        return
      else {
        conf1.close()
        conf2.close()
      }
    }

    val js = JobSpinner.jobSpinner[Boolean]("connect".green)
    Future {
      js.setMessage("connection pool for source..")
      val ok1= conf1.initDataSource("source", s => js.setMessage( "(S) " + s))

      js.setMessage("connection pool for target..")
      val ok2= conf2.initDataSource("target", s => js.setMessage( "(S) " + s))

      if (ok1 && ok2) {
        dataSource1 = conf1.dataSourceOr("source", show)
        dataSource2 = conf2.dataSourceOr("target", show)
        js.setFinished(true)
      } else {
        conf1.close()
        conf2.close()
        js.setFinished(false)
      }
    }

    val out = js
      .run(clearOnStart= false, clearOnExit = false, terminal= ioterm)
      .fold(
        e => {show( bullet + s"fail to connect : ${e.getMessage}"); false},
        r => r.getOr.getOrElse(false))

    if(out)
      show_cst
  }

  def selectNames(title: String, hdr: String, names: Seq[String]): Seq[String] = {

    val rows = names.map(n => Seq(Text(n)))
    val ss = MultiTable
      .multiTable(title,  Seq(hdr), rows)
      .run( clearOnStart=false, clearOnExit= false, terminal= Some(inst.term))
      .map(_.selected).getOrElse(Set.empty)

    val out = names.zipWithIndex.flatMap{ case (a, i) => if(ss.contains(i)) Some(a) else None}
    out
  }

  def selectTableNames(ds1: DataSource, schema1: String,
                       ds2: DataSource, schema2: String, all: Boolean): (Seq[String], Seq[String]) = {

    val names1 = fetchTableNames(ds1, schema1)
    val names2 = fetchTableNames(ds2, schema2)
    if(all) names1 -> names2 else {
      val t1 = selectNames("select tables", "Table", names1)
      val t2 = names2.filter(n => t1.contains(n))
      t1 -> t2
    }
  }

  def init(all: Boolean): Unit = {

    val ds = dataSourcesOr
    if ( ds.isEmpty) return
    val (ds1, ds2) = ds.get

    val cf1 = confOr(isA = true).get
    val cf2 = confOr(isA = false).get

    val js = JobSpinner.jobSpinner[SchemaCompared]("init".green)
    val (ns1, ns2) = selectTableNames( ds1, cf1.schema, ds2, cf2.schema, all)
    if(ns1.isEmpty) {
      show(bullet + "empty" )
      return
    }

    val start: Long = java.lang.System.currentTimeMillis()
    Future {
      val schema1 = fetchSchema(ds1, cf1.schema, ns1, s => js.setMessage("(S) " + s), 4)
      val schema2 = fetchSchema(ds2, cf2.schema, ns2, s => js.setMessage("(T) " + s), 4)
      val out = compareSchemas(cf1, cf2, schema1, schema2, s => js.setMessage("(F) " + s))
      js.setFinished( out)
    }

    val ret = js
      .run(clearOnStart= false, clearOnExit = false, terminal= ioterm)
      .fold(
        e => {show(bullet + "fail to init : " + e.getMessage ); None},
        r => r.getOr
      )

    for( o <- ret) {
      show(summary(o))
      Compared = Some(o)
      val end: Long = java.lang.System.currentTimeMillis()
      show(bullet + s"done: ${end-start} ms")
    }
  }

  def detail(l: List[TableInfo]) {
    if(l.isEmpty) { show(bullet + "not exist") } else {
      val ts = selectTables(l)
      ts.foreach( t => show(tableDetail(t)) )
    }
  }

  def save(path: String, pname: Option[String]): Boolean = {

    val planOr = comparePlanOr
    if(planOr.isEmpty)
      return false

    val pn = pname.getOrElse(
      readNotEmpty( inst.term, "", "? conf name(use as legal filename) ? ".green, Some("myConf"), false)
    )
    val fname = s"plan_$pn.conf"

    val out = Try {
      val p = Paths.get(path).resolve(fname)
      if (Files.exists(p)) throw new IllegalArgumentException(s"$fname already exist.")
      val j = planOr.get.toJsonPretty
      val b = j.getBytes(StandardCharsets.UTF_8)
      Files.write(p, b)
      show(bullet + s"conf is written to ${fname.green}")
    }.toEither

    out.left.foreach(e => show(bullet + s"fail to save($fname) ".red + e.getMessage) )
    out.isRight
  }

  def load0(path: String, pname: Option[String]): Option[SchemaCompared] = {

    val list = getFileList("plan_", ".conf", path).fold(
      e => { show(bullet + s"fail to locate plan conf: ${e.getMessage}"); List.empty},
      r => r
    )
    val pn = pname.map(n => s"plan_$n.conf" ).orElse{
      if(list.isEmpty) {
        show(bullet + s"conf file not exist."); None
        None
      } else  {
        SingleBox
          .singleBox("select plan", list)
          .run( clearOnStart= false, clearOnExit= false, terminal= Some(inst.term))
          .fold(
            e => {show(bullet + s"fail to select : ${e.getMessage}"); None},
            r => r.selectedItem)
      }
    }
    if (pn.isEmpty)
      return None

    val out = Try {
      val fname = pn.get
      val p = Paths.get(path).resolve(fname)
      val b = Files.readAllBytes(p)
      val s = new String(b, StandardCharsets.UTF_8)
      val o = s.fromJson[SchemaCompared]
      val fstr = fname.green
      o match {
        case Left(e) =>
          show(bullet + s"load fail. : $fstr : $e")
          None
        case Right(cp) =>
          show(bullet + s"load success. : $fstr")
          Some(cp)
      }
    }.toOption.flatten
    out
  }

  def setConfIfChanged(cp: SchemaCompared, ask: Boolean = false): Boolean = {

    val co = "co".green
    def showChanged = show(bullet + "must re-connect before proceed(connection setting changed.). use " + co)

    val changed = cp.conf1.neqUrlSchema(dbconf1) || cp.conf2.neqUrlSchema(dbconf2)

    if(changed) {
      if (ask) {
        val ok = askConfirm(inst.term, bullet + "connection setting changed. proceed anyway.")
        if (ok) {
          showChanged
          Compared = Some(cp)
          dbconf1 = Some(cp.conf1)
          dbconf2 = Some(cp.conf2)
          true
        } else
          false
      } else {
        showChanged
        Compared = Some(cp)
        dbconf1 = Some(cp.conf1)
        dbconf2 = Some(cp.conf2)
        true
      }
    } else {
      Compared = Some(cp)
      true
    }
  }

  def load(path: String, pname: Option[String]): Boolean = {
    val sc = load0(path, pname)
    sc.foreach{ cp => setConfIfChanged(cp) }
    sc.isDefined
  }

  def updateCount() {

    val planOr = comparePlanOr
    val ds = dataSourcesOr
    if( planOr.isEmpty || ds.isEmpty) return

    val sc = planOr.get
    val cs = sc.comparable
    val (ds1, ds2) = ds.get

    if(isEmpty(cs)) return

    val selected = selectTables(cs)
    val js = JobSpinner.jobSpinner[Seq[TableInfo]](s"fetch count of ${selected.size} tables".cyan)

    Future{
      js.setMessage("start")
      val update = selected.map{ s =>
        js.setMessage(s"${s.name}")
        s.fetchCount(ds1, ds2)
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

  def withCompared( f: SchemaCompared => Unit)
  : Unit = {
    comparePlanOr.foreach( p => { f(p) })
  }


  def selectPlansNotIn(names: Set[String]): Seq[ComparePlan] = {
    comparePlanOr.map{ cp =>
      val ts = selectTables(cp.comparable.filterNot(c => names.contains(c.name)))
      cp.comparePlans.filter(p => ts.exists( _.name == p.table.name) )
    }.getOrElse(Seq.empty)
  }

  def selectPlansOr: Option[List[ComparePlan]] = comparePlanOr.map { o =>
    val ts = selectTables(o.comparable)
    o.comparePlans.filter(p => ts.exists( _.name == p.table.name) )
  }

  def selectPlans(): Seq[ComparePlan] = selectPlansOr.getOrElse(Seq.empty)
  def selectPlan(): Option[ComparePlan] = comparePlanOr.flatMap{ o =>
    val tn = selectTable(o.comparable)
    tn.flatMap( n => o.comparePlans.find( p => p.name == n) )
  }
  def selectPlans0(o: SchemaCompared): Seq[ComparePlan] = {
    val ts = selectTables(o.comparable)
    o.comparePlans.filter(p => ts.exists( _.name == p.table.name) )
  }


}

case class TUIConfState( show: String => Unit, display: String => Unit, inst: RuntimeShellInstance)(implicit sema: ScreenSemaphore) {

  val tuiCon = TUIConnection(show, inst)
  def showRM(rm: ReportMsg) = { display(rm.statusString) }

  // --------------------------------------------------------------------------------
  def selectPlansNotIn(names: Set[String]): Seq[ComparePlan] = tuiCon.selectPlansNotIn(names)
  def selectPlansOr = tuiCon.selectPlansOr
  def selectPlans0(o: SchemaCompared): Seq[ComparePlan] = tuiCon.selectPlans0(o)

  def updateConSetting(kind: String, isA: Boolean) = tuiCon.updateConSetting(kind, isA)
  def connect() = tuiCon.connect()
  def connected = {
    val out = tuiCon.connected
    if(!out) show(bullet + "not connected. see " + "co".green)
    out
  }
  def updateCount() = tuiCon.updateCount()
  def setConfIfChanged(sc: SchemaCompared, ask: Boolean) = tuiCon.setConfIfChanged(sc, ask)
  def save(path: String = ".", pname: Option[String] = None) = tuiCon.save(path, pname)
  def load(path: String = ".", pname: Option[String] = None) = tuiCon.load(path, pname)
  def load0(path: String = ".", pname: Option[String] = None): Option[SchemaCompared] = tuiCon.load0(path, pname)
  def comparedDataSourcesOr: Option[(DataSource, DataSource)] = tuiCon.comparedDataSourcesOr
  // --------------------------------------------------------------------------------

  private def showAllPlan(o: SchemaCompared) = {
    val selected = selectPlans0(o)
    selected.foreach(p => p.display(show) )
  }

  private def withCompared( f: SchemaCompared => Unit): Unit = tuiCon.withCompared(f)
  private def detail(l: List[TableInfo]) = tuiCon.detail(l)

  def start_init(all: Boolean) = tuiCon.init(all)
  def show_st    = tuiCon.show_st
  def show_cst   = tuiCon.show_cst
  def show_b     = withCompared(o => show(summary(o)))
  def show_mka   = withCompared(_.show_mka(show))
  def show_mkb   = withCompared(_.show_mkb(show))
  def show_mca   = withCompared(_.show_mca(show))
  def show_mcb   = withCompared(_.show_mcb(show))
  def show_oa    = withCompared(_.show_oa (show))
  def show_ob    = withCompared(_.show_ob (show))
  def show_pst   = tuiCon.show_pst
  def show_l     = withCompared(_.show_l  (show))
  def show_ln    = withCompared(_.show_ln (show))
  def show_lk    = withCompared(_.show_lk (show))
  def show_mkad  = withCompared(o => detail(o.mismatchKey.map(_._1)))
  def show_mkbd  = withCompared(o => detail(o.mismatchKey.map(_._2)))
  def show_mcad  = withCompared(o => detail(o.mismatchCols.map(_._1)))
  def show_mcbd  = withCompared(o => detail(o.mismatchCols.map(_._2)))
  def show_oad   = withCompared(o => detail(o.onlyInDb1))
  def show_obd   = withCompared(o => detail(o.onlyInDb2))
  def show_d     = withCompared(o => detail(o.comparable))
  def show_dn    = withCompared(o => detail(o.filterNoKey))
  def show_dk    = withCompared(o => detail(o.filterKey))
  def show_plan  = withCompared(o => showAllPlan(o))

  // compare --------------------------------
  def setWhere: Unit = {
    val cp = tuiCon.selectPlan()
    if(cp.isEmpty) return

    val ti = cp.get
    ti.display(show)
    val where = "WHERE ".green
    val in = InputPrompt.readLine(inst.term, bullet + where)
    val ok = askConfirm(inst.term, where + in + "\n")
    if(ok) {
      tuiCon.setWhere(ti.name, in)
      ti.display(show)    // <<<< todo
    }
  }

  val no_cancel = () => false

  // compare --------------------------------
  def start_ps(n: Option[Int], debug: Boolean = false) {

    n.foreach( i => show(bullet + s"process first ${i.toString.yellow} entries."))
    comparedDataSourcesOr.foreach { case (ds1, ds2) =>
      val selected = tuiCon.selectPlans()
      for (p <- selected) {
        show(rowElements(p.table).map(_.render).mkString(" "))
        p.goCompare(
          s1 = ds1,
          s2 = ds2,
          limit = n,
          notice = showRM,
          compDebug = debug)
      }
    }
  }

  def kindFilter(kind: Option[String])
  : DiffRow => Boolean = kind match {
    case None => DiffRow.isNotSame
    case Some(ks) =>
      val opts = List(
        Option.when(ks.contains("i"))(DiffRow.isOnlyA  ->("Insert(" + "onlyInA".yellow +")")),
        Option.when(ks.contains("u"))(DiffRow.isUpdate ->("Update(" + "changed".yellow +")")),
        Option.when(ks.contains("d"))(DiffRow.isOnlyB  ->("Delete(" + "onlyInB".yellow +")")),
      ).flatten

      if(opts.isEmpty)
        DiffRow.isNotSame
      else {
        val prefix = bullet + "Operate on " + "selected types only".yellow + ". \n" + bullet + "types: "
        val msg = opts.map(_._2).mkString(prefix, ", ", "")
        val ok = askConfirm(inst.term, msg)
        if (!ok) {
          DiffRow.isNotSame
        } else {
          tuiLogger.info(msg)
          val fs = opts.map(_._1)
          (dr: DiffRow) => fs.exists(f => f(dr))
        }
      }
  }

  // compare & apply ------------------------
  def start_pa(n: Option[Int], kind: Option[String] = None, compDebug: Boolean = false, applDebug: Boolean = false , mock: Boolean = false) {

    n.foreach( i => show(bullet + s"process first ${i.toString.yellow} entries."))

    comparedDataSourcesOr.foreach { case (ds1, ds2) =>
      val selected = tuiCon.selectPlans()
      if(selected.isEmpty) return

      val pred: DiffRow => Boolean = kindFilter(kind)
      if( !mock) {
        val confirm = askConfirm(inst.term, bullet + "Comparison results will be applied to target.")
        if(!confirm) return
      }

      for (p <- selected) {
        show(rowElements(p.table).map(_.render).mkString(" "))
        p.goCompareApply(
          s1 = ds1,
          s2 = ds2,
          limit = n,
          notice = showRM,
          filterPred = pred,
          mock = mock,
          compDebug = compDebug,
          applDebug = applDebug)
      }
    }
  }
}

