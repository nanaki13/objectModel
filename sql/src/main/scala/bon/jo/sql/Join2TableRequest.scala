package bon.jo.sql
import bon.jo.common.typeutils.~

object Join2TableRequest:
  inline def joinRequest[L, R, T[_, _] <: JoinType[_, _]]
      : Join2TableRequest[L, R, T] ?=> Join2TableRequest[L, R, T] = summon
  inline def joinRequest3[T1, T2, T3, J1[_, _] <: JoinType[_, _], J2[
      _,
      _
  ] <: JoinType[_, _]]: ~[Join3TableRequest[T1, T2, T3, J1, J2]] = summon


trait Join2TableRequest[L, R, T[_, _] <: JoinType[_, _]](using
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