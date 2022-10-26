package bon.jo.sql

case class JoinDef[L, R](
    joinType: JoinType[L, R],
    on: (leftAlias: String, rightAlias: String) => String
)
object JoinDef:
  def on[L, R](
      leftAlias: String,
      rightAlias: String
  ): JoinDef[L, R] ?=> String = summon.on(leftAlias, rightAlias)
