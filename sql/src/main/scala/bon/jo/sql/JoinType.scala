package bon.jo.sql

enum JoinType[L, R]:
  case Default()
  case Left()
  case Right()
  def sql: String =
    this match
      case JoinType.Default() => ""
      case JoinType.Left()    => "LEFT"
      case JoinType.Right()   => "RIGHT"
object JoinType:
  def joinType[L, R]: JoinType[L, R] ?=> String = summon.sql
  given [L, R](using JoinDef[L, R]): JoinType[L, R] = summon.joinType