package bon.jo.service

import bon.jo.sql.ResultSetMapping
import java.sql.ResultSet
import bon.jo.model.SubjectModel
import java.sql.PreparedStatement
import bon.jo.sql.Service
import bon.jo.sql.Sort
import bon.jo.sql.Sort.{asc,desc}
import bon.jo.sql.PSMapping
import bon.jo.sql.stmtSetObject
import bon.jo.sql.stmt
import java.sql.Connection
import java.time.LocalDateTime
import bon.jo.sql.BaseSqlRequest
import bon.jo.sql.Join2TableRequest
import bon.jo.sql.Join2TableService
import bon.jo.domain.User
import bon.jo.sql.Alias
import bon.jo.domain
import bon.jo.model.SubjectModel.Subject

import java.time.ZoneId
import bon.jo.sql.Limit
object SqlServiceSubject {

  type ServiceSubject = Service[Subject, Int] with SqlServiceSubject
  def toLocalDateTime(a: Any): LocalDateTime =
    a match
      case e: LocalDateTime => e
      case o:       java.sql.Timestamp => LocalDateTime.ofInstant( o.toInstant(),ZoneId.systemDefault())
      case o                => 
        LocalDateTime.parse(o.toString())

  given ResultSetMapping[Subject] =
    (from, r) =>
       Subject(id = r.getInt(from), subjectDateTime= toLocalDateTime( r.getObject(from+1)), title= r.getString(from+2), description= r.getString(from+3))
  given ResultSetMapping[Int] = (from, r) =>
    r.getInt(from)
  given BaseSqlRequest[Subject] = BaseSqlRequest[Subject](SubjectModel.table)
  given PSMapping[Subject] with
    def apply(from: Int, v: Subject)(using PreparedStatement): Int =
      stmtSetObject(from, v.id)
      stmtSetObject(from + 1, v.subjectDateTime)
      stmtSetObject(from + 2, v.title)
      stmtSetObject(from + 3, v.description)
      from + 4
  given PSMapping[Int] with
    def apply(from: Int, v: Int)(using PreparedStatement): Int =
      stmtSetObject(from, v)
    
      from + 1
  inline def apply()(using () => Connection): ServiceSubject =
    new Service[Subject, Int] with SqlServiceSubject

}
trait SqlServiceSubject:
  self: Service[Subject, Int] =>
  import SqlServiceSubject.given
  import self.given
  given (() => Connection) = connection
  given Alias = Alias()
 

  

