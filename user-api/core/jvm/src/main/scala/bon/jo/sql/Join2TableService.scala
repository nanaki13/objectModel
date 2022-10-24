package bon.jo.sql

import java.sql.Connection
import bon.jo.sql.UsingCo
import bon.jo.sql.stmtSetObject
import java.sql.PreparedStatement
import bon.jo.sql.ResultSetMapping
import bon.jo.sql.executeQuery
import bon.jo.sql.iterator
import bon.jo.sql.SortOps.sql
import bon.jo.common.Anys.toSome

trait Join2TableService[L, R, T[_, _] <: JoinType[_, _]](
    using() => Connection,
    Join2TableRequest[L, R, T]
) extends UsingCo with SelectOps:
  import Join2TableRequest.joinRequest
  val req = Join2TableRequest.joinRequest
  type Ret = req.Ret
  lazy val joinCondition: String
  lazy val sqlBaseSelect = joinRequest.select + " ON " + joinCondition
