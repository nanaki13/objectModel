package bon.jo.sql

import java.sql.Connection
import bon.jo.sql.StringWriter.*
import java.sql.PreparedStatement
import java.sql.ResultSet
import bon.jo.sql.Sql.PSMapping
import bon.jo.common.typeutils.~
import bon.jo.sql.Sql.BaseSqlRequest
import bon.jo.common.Anys.toSome
import BaseSqlRequest.baseSqlRequest
import java.sql.Statement
import Sql.Sort.sql
import Sql.Sort
object Sql {
  case class Sort(column: String, dir: Dir)
  enum Dir:
    case ASC, DESC
  case class Builder(
      var value: Table,
      val id: ColumnsRef = ColumnsRef(),
      var indexs: Seq[Index] = Nil,
      var autoInc: Seq[String] = Nil
  )
  case class BuilderCol(var value: Column)
  type ColumnInTable = Builder ?=> Column
  type OnBuildColumnInTable = (Builder, BuilderCol) ?=> Unit
  type OnBuild = Builder ?=> Unit
  type OnBuildCol = BuilderCol ?=> Unit
  inline def tableBuilder: Builder ?=> Builder = summon
  inline def columnBuilder: BuilderCol ?=> BuilderCol = summon
  inline def tableid: Builder ?=> ColumnsRef = tableBuilder.id
  extension (r: ResultSet)
    def iterator: Iterator[ResultSet] = new Iterator[ResultSet] {
      def hasNext: Boolean = r.next()
      def next(): ResultSet = r
    }
  class ColumnsRef(var values: Seq[String] = Nil)
  enum IndexType:
    case Simple, Unique
  case class Index(
      var values: Seq[String] = Nil,
      indexType: IndexType = IndexType.Simple
  )

  object Table:
    def tableName(name: String): OnBuild =
      summon.value = summon.value.copy(name)
    def id: OnBuildColumnInTable =
      tableid.values = tableid.values :+ columnBuilder.value.name
    def autoIncr: OnBuildColumnInTable =
      tableBuilder.autoInc = tableBuilder.autoInc :+ columnBuilder.value.name
    def index: OnBuildColumnInTable =
      tableBuilder.indexs =
        tableBuilder.indexs :+ Index(columnBuilder.value.name :: Nil)
    def unique: OnBuildColumnInTable =
      tableBuilder.indexs = tableBuilder.indexs :+ Index(
        columnBuilder.value.name :: Nil,
        IndexType.Unique
      )

    def apply(f: OnBuild): Table =
      given Builder = Builder(Table("", Nil, Nil, Nil, Set.empty))
      f

      summon.value.copy(
        id = tableBuilder.id.values,
        indexs = tableBuilder.indexs,
        autoIncr = tableBuilder.autoInc.toSet
      )
    def col(f: OnBuildCol): OnBuild =
      given BuilderCol = BuilderCol(Column("", ""))
      f
      summon.value
  case class Table(
      name: String,
      columns: Seq[Column],
      id: Seq[String],
      indexs: Seq[Index],
      autoIncr: Set[String]
  ):
    def createSql_ : String =
      s"""CREATE TABLE $name(
        ${columns.mkString(",\n        ")},
        PRIMARY KEY(${id.mkString(",")}));
        ${indexs
          .map { index =>
            val indexType = index.indexType match
              case IndexType.Unique => "UNIQUE"
              case _                => ""

            s"""CREATE $indexType INDEX ${index.values.mkString(
                "_"
              )}_idx ON $name(${index.values.mkString(",")});"""
          }
          .mkString("\n")}"""
    def createSql(using dbTypeOf: DBType): String =
      given Set[String] = autoIncr
      s"""CREATE TABLE $name(
        ${columns.map(_.sqlDef).mkString(",\n        ")}
        ${
          if dbTypeOf == DBType.SQLite && autoIncr.nonEmpty then ""
          else s""",PRIMARY KEY(${id.mkString(",")})"""
        }
        );
        ${indexs
          .map { index =>
            val indexType = index.indexType match
              case IndexType.Unique => "UNIQUE"
              case _                => ""

            s"""CREATE $indexType INDEX ${name}_${index.values.mkString(
                "_"
              )}_idx ON $name(${index.values.mkString(",")});"""
          }
          .mkString("\n")}"""
  case class Column(name: String, dbType: String):
    override def toString() = s"$name $dbType"
    inline def isAi(using ai: Set[String]) = ai.contains(name)
    def sqlDef(using ai: Set[String], dbTypeOf: DBType): String =

      val dBDependType =   dbTypeOf match
          case DBType.PostgreSQL if isAi =>
            val aiType = dbType match
              case "INT"    => "SERIAL"
              case "BIGINT" => "BIGSERIAL"
            s"$aiType"

          case DBType.SQLite if isAi  => "INTEGER PRIMARY KEY AUTOINCREMENT"
          case  DBType.PostgreSQL if dbType == "BLOB" => "bytea"
          case _ => dbType
      s"$name $dBDependType"
     

  object Column:
    def columnName(name: String): OnBuildCol =
      summon.value = summon.value.copy(name)
    def _type(name: String): OnBuildCol =
      summon.value = summon.value.copy(dbType = name)
    def apply(f: OnBuildCol*): ColumnInTable =
      given BuilderCol = BuilderCol(Column("", ""))
      f.foreach(ff => ff)
      val t = tableBuilder.value
      val c = columnBuilder.value
      tableBuilder.value = t.copy(columns = t.columns :+ c)
      c
  inline def prepare(sql: String)(using Connection): PreparedStatement =
    summon.prepareStatement(sql)

  inline def stmtSetObject(i: Int, o: Any)(using PreparedStatement) =
    summon.setObject(i, o)
  inline def executeQuery()(using PreparedStatement): ResultSet =
    summon.executeQuery()
  inline def executeUpdate()(using PreparedStatement): Int =
    summon.executeUpdate()
  inline def execute()(using PreparedStatement): Boolean = summon.execute()
  inline def stmtClose(using Statement) = summon.close

  def doSql[A](sql: String)(f: PreparedStatement ?=> A)(using Connection): A =

    given PreparedStatement = prepare(sql)
    val rest = f
    stmtClose
    rest
  inline def stmt: Statement ?=> Statement = summon
  def stmtDo[A]()(f: Statement ?=> A)(using c: Connection): A =
    given Statement = c.createStatement()
    val rest = f
    stmtClose
    rest

  trait CountColumns[T]:
    def count: Int
  object CountColumns:
    inline def apply[T](using CountColumns[T]) = summon
    given [L](using JoinBaseSqlRequest[L, _, _]): CountColumns[L] =
      summon.leftTable
  trait ResultSetMapping[T]:
    def apply(from: Int, v: ResultSet): T
    inline def apply(v: ResultSet): T = this(1, v)
  object ResultSetMapping:
    inline def apply[T](using ResultSetMapping[T]) = summon
    given [L, R](using
        ResultSetMapping[L],
        ResultSetMapping[R],
        CountColumns[L]
    ): ResultSetMapping[(L, R)] with
      def apply(from: Int, v: ResultSet): (L, R) =
        (
          ResultSetMapping[L](from, v),
          ResultSetMapping[R](from + CountColumns[L].count, v)
        )
    given [L, R](using
        ResultSetMapping[L],
        ResultSetMapping[R],
        CountColumns[L]
    ): ResultSetMapping[(L, Option[R])] with
      def apply(from: Int, v: ResultSet): (L, Option[R]) =
        val opt = Option(v.getObject(from + CountColumns[L].count)).map { _ =>
          ResultSetMapping[R](from + CountColumns[L].count, v)
        }
        (ResultSetMapping[L](from, v), opt)
    given [T1, T2, T3](using
        ResultSetMapping[T1],
        ResultSetMapping[T2],
        ResultSetMapping[T3],
        CountColumns[T1],
        CountColumns[T2]
    ): ResultSetMapping[(T1, T2, T3)] with
      def apply(from: Int, v: ResultSet): (T1, T2, T3) =
        (
          ResultSetMapping[T1](from, v),
          ResultSetMapping[T2](from + CountColumns[T1].count, v),
          ResultSetMapping[T3](from + CountColumns[T1].count + CountColumns[T2].count, v)
        )

  trait PSMapping[T]:
    def apply(from: Int, v: T)(using PreparedStatement): Int
    def fillCreate(from: Int, v: T)(using PreparedStatement): Int =
      apply(from, v)
  object PSMapping:
    inline def apply[T](using PSMapping[T]) = summon

  inline def mapping[T]: ResultSetMapping[T] ?=> ResultSetMapping[T] = summon

  object BaseSqlRequest:
    inline def baseSqlRequest[T](using BaseSqlRequest[T]) = summon
    inline def table[T](using BaseSqlRequest[T]): Table = baseSqlRequest.table
    def apply[T](table: Table): BaseSqlRequest[T] = Impl[T](table)
    private case class Impl[T](table: Table) extends BaseSqlRequest[T]
  trait BaseSqlRequest[T] extends CountColumns[T]:
    val table: Table
    val count = table.columns.size
    def columnsString = table.columns.map(_.name).mkString(", ")
    def columnsStringWithoutAutoInc = table.columns
      .map(_.name)
      .filter(cName => !table.autoIncr.contains(cName))
      .mkString(", ")
    def aliasDotcolumnsString(alias: String) =
      table.columns.map(col => s"$alias.${col.name}").mkString(", ")
    def idsString = table.id.mkString(", ")
    def aliasDotidsString(alias: String) =
      table.id.map(col => s"$alias.$col").mkString(", ")
    def idsConditionString =
      s" ${table.id.map(idCol => s"$idCol = ?").mkString(" AND ")} "
    def aliasDotidsConditionString(alias: String) =
      s" ${table.id.map(idCol => s"$alias.$idCol = ?").mkString(" AND ")} "
    def columnsParamString = table.columns.map(_ => "?").mkString(", ")
    def columnsParamStringWithoutAutoInc = table.columns
      .filter(c => !table.autoIncr.contains(c.name))
      .map(_ => "?")
      .mkString(", ")
    def updateSetString: String =
      table.columns.map(c => s"${c.name} = ?").mkString(", ")
    val sqlBaseSelect = s"""SELECT $columnsString FROM ${table.name}"""
    val selectByIdString =
      s"""SELECT $columnsString FROM ${table.name} WHERE $idsConditionString"""
    val containsString = s"""SELECT 1 FROM ${table.name}"""
    val containsByIdString = s"""$containsString WHERE ${idsConditionString}"""
    val selectColumnIdIndex = table.id.zipWithIndex.toMap
    val columnIdIndex = table.columns.zipWithIndex.toMap
    val deleteByIdString =
      s"""DELETE FROM ${table.name} WHERE ${idsConditionString}"""
    val updateByIdString =
      s"""UPDATE  ${table.name} SET ${updateSetString}  WHERE $idsConditionString"""
    val insertString =
      s"""INSERT INTO ${table.name} ( $columnsStringWithoutAutoInc) VALUES ( ${columnsParamStringWithoutAutoInc} )"""

  class Alias:
    import scala.collection.mutable
    val givenAlias: mutable.Map[String, Int] = mutable.Map.empty
    def apply(s: String): String =
      val pref = givenAlias.getOrElseUpdate(s, 1)
      givenAlias += s -> (pref + 1)
      s + "_" + (pref)
  object Alias:
    inline def alias: Alias ?=> Alias = summon

  object JoinBaseSqlRequest:
    inline def joinRequest[L, R, T[_, _] <: JoinType[_, _]]
        : JoinBaseSqlRequest[L, R, T] ?=> JoinBaseSqlRequest[L, R, T] = summon
    inline def joinRequest3[T1, T2, T3, J1[_, _] <: JoinType[_, _], J2[
        _,
        _
    ] <: JoinType[_, _]]: ~[T1JoinT2JoinT3Request[T1, T2, T3, J1, J2]] = summon

  enum JoinType[L, R]:
    case Default()
    case Left()
    case Right()
    def sql: String =
      this match
        case JoinType.Default() => ""
        case JoinType.Left()    => "LEFT"
        case JoinType.Right()   => "RIGHT"
  case class JoinDef[L, R](
      joinType: JoinType[L, R],
      on: (leftAlias: String, rightAlias: String) => String
  )
  object JoinDef:
    def on[L, R](
        leftAlias: String,
        rightAlias: String
    ): JoinDef[L, R] ?=> String = summon.on(leftAlias, rightAlias)
  object JoinType:
    def joinType[L, R]: JoinType[L, R] ?=> String = summon.sql
    given [L, R](using JoinDef[L, R]): JoinType[L, R] = summon.joinType
  type RetC[J1[_, _], T1, T2] = J1[T1, T2] match
    case JoinType.Default[T1, T2] => (T1, T2)
    case JoinType.Left[T1, T2]    => (T1, Option[T2])
    case JoinType.Right[T1, T2]   => (Option[T1], T2)
  trait T1JoinT2JoinT3Request[T1, T2, T3, J1[_, _] <: JoinType[_, _], J2[
      _,
      _
  ] <: JoinType[_, _]](using
      BaseSqlRequest[T1],
      BaseSqlRequest[T2],
      BaseSqlRequest[T3],
      JoinDef[T1, T2],
      JoinDef[T2, T3],
      Alias
  ):
    import Alias.alias
    import BaseSqlRequest.table
    import JoinType.joinType
    import JoinDef.on

    inline def t1Table = BaseSqlRequest.baseSqlRequest[T1]
    inline def t2Table = BaseSqlRequest.baseSqlRequest[T2]
    inline def t3Table = BaseSqlRequest.baseSqlRequest[T3]
    type Ret1 = RetC[J1, T1, T2]
    type Ret2 = RetC[J2, T2, T3]
    type Ret = (Ret1, Ret2) match
      case ((T1, T2), (T2, T3))                 => (T1, T2, T3)
      case ((T1, Option[T2]), (Option[T2], T3)) => (T1, Option[T2], Option[T3])
      case ((T1, T2), (T2, Option[T3]))         => (T1, T2, Option[T3])
    given CountColumns[T1] = t1Table
    given CountColumns[T2] = t2Table
    val t1Alias = alias(table[T1].name)
    val t2Alias = alias(table[T2].name)
    val t3Alias = alias(table[T3].name)
    val t1AliasedTable = table[T1].name + " " + t1Alias
    val t2AliasedTable = table[T2].name + " " + t2Alias
    val t3AliasedTable = table[T3].name + " " + t3Alias
    def columns: String =
      t1Table.aliasDotcolumnsString(t1Alias) + ", " + t2Table
        .aliasDotcolumnsString(t2Alias) + ", " + t3Table.aliasDotcolumnsString(
        t3Alias
      )
    def select: String =
      s"SELECT $columns FROM $t1AliasedTable $join12 ON ${on[T1, T2](t1Alias, t2Alias)} $join23 ON ${on[T2, T3](t2Alias, t3Alias)} "
    def join12: String = s"${joinType[T1, T2]} JOIN $t2AliasedTable"
    def join23: String = s"${joinType[T2, T3]} JOIN $t3AliasedTable"

  object T1JoinT2JoinT3Service:
    def apply[T1, T2, T3, J1[_, _] <: JoinType[_, _], J2[_, _] <: JoinType[
      _,
      _
    ]]()(
        using() => Connection,
        BaseSqlRequest[T1],
        BaseSqlRequest[T2],
        BaseSqlRequest[T3],
        JoinDef[T1, T2],
        JoinDef[T2, T3],
        Alias
    ): T1JoinT2JoinT3Service[T1, T2, T3, J1, J2] =
      given T1JoinT2JoinT3Request[T1, T2, T3, J1, J2] =
        new T1JoinT2JoinT3Request {}
      new T1JoinT2JoinT3Service {}

  trait T1JoinT2JoinT3Service[T1, T2, T3, J1[_, _] <: JoinType[_, _], J2[
      _,
      _
  ] <: JoinType[_, _]](
      using() => Connection,
      T1JoinT2JoinT3Request[T1, T2, T3, J1, J2]
  ) extends UsingCo  with SelectOps:
    import JoinBaseSqlRequest.joinRequest3
    val request = JoinBaseSqlRequest.joinRequest3
    import request.given
    type Ret = request.Ret

    lazy val sqlBaseSelect = joinRequest3.select



  trait JoinBaseSqlRequest[L, R, T[_, _] <: JoinType[_, _]](using
      BaseSqlRequest[L],
      BaseSqlRequest[R],
      JoinType[L, R],
      Alias
  ):

    import Alias.alias
    import BaseSqlRequest.table
    import JoinType.joinType
    type Ret = T[L, R] match
      case JoinType.Default[L, R] => (L, R)
      case JoinType.Left[L, R]    => (L, Option[R])
      case JoinType.Right[L, R]   => (Option[L], R)
    inline def leftTable = BaseSqlRequest.baseSqlRequest[L]
    inline def rightTable = BaseSqlRequest.baseSqlRequest[R]
    val leftAlias = alias(table[L].name)
    val rightAlias = alias(table[R].name)
    val leftAliasedTable = table[L].name + " " + leftAlias
    val rightAliasedTable = table[R].name + " " + rightAlias
    def columns: String = leftTable.aliasDotcolumnsString(
      leftAlias
    ) + ", " + rightTable.aliasDotcolumnsString(rightAlias)
    def select: String = s"SELECT $columns FROM $join"
    def join: String =
      s"$leftAliasedTable ${joinType[L, R]} JOIN $rightAliasedTable"

  trait UsingCo(using() => Connection):
    def connection: () => Connection = summon
    def sql[A](sql: String)(f: PreparedStatement ?=> A): A =
      doSql(sql)(f)(using summon[() => Connection]())

  object Sort:
    val asc: (column: String) => Sort = Sort(_, Dir.ASC)
    val desc: (column: String) => Sort = Sort(_, Dir.DESC)
    extension (sorts: Seq[Sort])
      def sql: String =
        if sorts.isEmpty then ""
        else " ORDER BY " + sorts.map(e => e.column + " " + e.dir).mkString(",")
  enum Limit:
    case NoLimit
    case Fixed(from: Long, size: Int)
  extension (l: Limit)
    def toSql: String =
      l match
        case Limit.NoLimit           => ""
        case Limit.Fixed(from, size) => s"LIMIT ${size} OFFSET ${from}"

  trait JoinService[L, R, T[_, _] <: JoinType[_, _]](
      using() => Connection,
      JoinBaseSqlRequest[L, R, T]
  ) extends UsingCo with SelectOps:
    import JoinBaseSqlRequest.joinRequest
    val req = JoinBaseSqlRequest.joinRequest
    type Ret = req.Ret
    lazy val joinCondition: String
    lazy val sqlBaseSelect = joinRequest.select + " ON " + joinCondition
  trait SelectOps:
    sel : UsingCo =>
      lazy val sqlBaseSelect : String
      type Ret 
      def bindToStmt(fieldvalue: Seq[(String, Any)]): PreparedStatement ?=> Unit =
        fieldvalue
          .map(_._2)
          .zipWithIndex
          .foreach((e, i) => stmtSetObject(i + 1, e))
      def paramsQ(fieldvalue: Seq[(String, Any)]) =
        fieldvalue.map(_._1).map(f => s" $f = ?").mkString(" AND ")
      def select(fieldvalue: Seq[(String, Any)]) =
        sqlBaseSelect + s" WHERE ${paramsQ(fieldvalue)}"
      def findAllBys(fieldvalue: (String, Any)*)(
          sorts: Seq[Sort] = Nil,
          limit: Limit = Limit.NoLimit
      ): ResultSetMapping[Ret] ?=> Seq[Ret] =

        val sqlS = select(fieldvalue) + sorts.sql + " " + limit.toSql
        sql(sqlS) {
          bindToStmt(fieldvalue)
          val r = executeQuery()
          r.iterator
            .map(r => {

              ResultSetMapping[Ret](1, r)
            })
            .toSeq
        }
      def findBys(
          fieldvalue: (String, Any)*
      ): ResultSetMapping[Ret] ?=> Option[Ret] =

        val sqlS = select(fieldvalue)
        println(sqlS)
        sql(sqlS) {
          bindToStmt(fieldvalue)
          val r = executeQuery()
          if r.next() then ResultSetMapping[Ret](1, r).toSome else None

        }

  trait Service[T, ID](
      using() => Connection,
      ResultSetMapping[ID],
      ResultSetMapping[T],
      BaseSqlRequest[T],
      PSMapping[T],
      PSMapping[ID]
  ) extends UsingCo:

    inline def request = baseSqlRequest
    def findBy(field: String, value: Any): Option[T] =
      sql(baseSqlRequest.sqlBaseSelect + s" WHERE $field = ?") {
        stmtSetObject(1, value)
        val r = executeQuery()
        if r.next() then Some(ResultSetMapping[T](1, r))
        else None
      }
    def findBys(fieldvalue: (String, Any)*)(sorts: Seq[Sort] = Nil): Seq[T] =
      val paramsQ = fieldvalue.map(_._1).map(f => s" $f = ?").mkString(" AND ")
      sql(baseSqlRequest.sqlBaseSelect + s" WHERE $paramsQ ${sorts.sql}") {
        fieldvalue
          .map(_._2)
          .zipWithIndex
          .foreach((e, i) => stmtSetObject(i + 1, e))

        val r = executeQuery()
        r.iterator.map(r => ResultSetMapping[T](r)).toSeq
      }
    def contains(ids: ID): Boolean =
      sql(baseSqlRequest.containsByIdString) {
        PSMapping[ID](1, ids)
        val r = executeQuery()
        r.next()
      }
    def contains(fieldvalue: (String, Any)*): Boolean =
      val paramsQ = fieldvalue.map(_._1).map(f => s" $f = ?").mkString(" AND ")
      sql(baseSqlRequest.containsString + s" WHERE $paramsQ") {
        fieldvalue
          .map(_._2)
          .zipWithIndex
          .foreach((e, i) => stmtSetObject(i + 1, e))
        val r = executeQuery()
        r.next()
      }
    def maxId(): ID =
      sql[ID](
        s"SELECT MAX(${baseSqlRequest.table.id.mkString(", ")}) FROM " + baseSqlRequest.table.name
      ) {
        val r = executeQuery()
        r.next()
        ResultSetMapping[ID](r)
      }

    def read(ids: ID): T =
      sql[T](baseSqlRequest.selectByIdString) {
        PSMapping[ID](1, ids)
        val r = executeQuery()
        r.next()
        ResultSetMapping[T](r)
      }
    def readOption(ids: ID): Option[T] =
      sql(baseSqlRequest.selectByIdString) {
        PSMapping[ID](1, ids)
        val r = executeQuery()
        if r.next() then Some(ResultSetMapping[T](r))
        else None
      }
    def delete(ids: ID): Unit = sql(baseSqlRequest.deleteByIdString) {
      PSMapping[ID](1, ids)
      executeUpdate()
    }
    def update(id: ID, t: T): Unit =
      sql[T](baseSqlRequest.updateByIdString) {
        PSMapping[ID](PSMapping[T](1, t), id)
        executeUpdate()
        t
      }
    def create(t: T): T =
      sql[T](baseSqlRequest.insertString) {
        PSMapping[T].fillCreate(1, t)
        executeUpdate()
        t
      }

  object Service:
    class Impl[T, ID](
        using() => Connection,
        ResultSetMapping[ID],
        ResultSetMapping[T],
        BaseSqlRequest[T],
        PSMapping[T],
        PSMapping[ID]
    ) extends Service[T, ID]
    def apply[T, ID](
        using() => Connection,
        ResultSetMapping[ID],
        ResultSetMapping[T],
        BaseSqlRequest[T],
        PSMapping[T],
        PSMapping[ID]
    ): Service[T, ID] = Impl()

}
