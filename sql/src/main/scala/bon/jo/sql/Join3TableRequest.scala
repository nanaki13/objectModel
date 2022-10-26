package bon.jo.sql
import bon.jo.sql.RetC
import bon.jo.sql.CountColumns

trait Join3TableRequest[T1, T2, T3, J1[_, _] <: JoinType[_, _], J2[
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

