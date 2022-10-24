package bon.jo.sql

import java.sql.Connection
import bon.jo.sql.UsingCo

object Join3TableService:
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
  ): Join3TableService[T1, T2, T3, J1, J2] =
    given Join3TableRequest[T1, T2, T3, J1, J2] =
      new Join3TableRequest {}
    new Join3TableService {}

trait Join3TableService[T1, T2, T3, J1[_, _] <: JoinType[_, _], J2[
    _,
    _
] <: JoinType[_, _]](
    using() => Connection,
    Join3TableRequest[T1, T2, T3, J1, J2]
) extends UsingCo  with SelectOps:
  import Join2TableRequest.joinRequest3
  val request = Join2TableRequest.joinRequest3
  import request.given
  type Ret = request.Ret

  lazy val sqlBaseSelect = joinRequest3.select

