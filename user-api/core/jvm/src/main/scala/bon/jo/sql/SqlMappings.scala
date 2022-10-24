package bon.jo.sql

import bon.jo.sql.PSMapping
import java.sql.PreparedStatement
import bon.jo.sql.stmtSetObject
import bon.jo.sql.ResultSetMapping
import bon.jo.sql.CountColumns
import java.sql.ResultSet

object SqlMappings:
  given PSMapping[Long] with
    def apply(from : Int,v : Long)(using PreparedStatement):Int=
      stmtSetObject(from,v)
      from+1
  given ResultSetMapping[Long] with
    def apply(from : Int,r : ResultSet):Long = 
      r.getLong(from).asInstanceOf[Long]

  given  [A] (using rB : ResultSetMapping[A] ) : ResultSetMapping[Option[A]] with
    def apply(from : Int,r : ResultSet):Option[A] = 
      Option(r.getObject(from)).map{
        _ => rB.apply(from,r)
      }

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
