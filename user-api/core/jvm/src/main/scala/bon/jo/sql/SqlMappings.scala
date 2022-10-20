package bon.jo.sql

import bon.jo.sql.Sql.PSMapping
import java.sql.PreparedStatement
import bon.jo.sql.Sql.stmtSetObject
import bon.jo.sql.Sql.ResultSetMapping
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
      println("-->  "+r.getObject(from))
      Option(r.getObject(from)).map{
        _ => rB.apply(from,r)
      }