package bon.jo.image

import bon.jo.sql.Sql.ResultSetMapping
import java.sql.ResultSet
import bon.jo.domain.Image
import java.sql.PreparedStatement
import bon.jo.sql.Sql.Service
import bon.jo.sql.Sql.BaseSqlRequest
import bon.jo.sql.Sql.PSMapping
import bon.jo.sql.Sql.stmtSetObject
import java.sql.Connection
object SqlServiceImage {
  
  type ServiceImage = Service[Image,Long] with SqlServiceImage
  given BaseSqlRequest[Image] = BaseSqlRequest[Image](ImageModel.userTable)
  given ResultSetMapping[Image] with
    def apply(from : Int,r : ResultSet):Image = 
      Image(r.getLong(from),r.getString(from+1),r.getBytes(from+2))
  given ResultSetMapping[Long] with
    def apply(from : Int,r : ResultSet):Long = 
      r.getLong(from).asInstanceOf
  given PSMapping[Image] with
    def apply(from : Int,v : Image)(using PreparedStatement):Int=
      stmtSetObject(from,v.id)
      stmtSetObject(from+1,v.name)
      stmtSetObject(from+2,v.data)
      from+3
    def create(from : Int,v : Image)(using PreparedStatement):Int=
      stmtSetObject(from,v.name)
      stmtSetObject(from+1,v.data)
      from+2
  given PSMapping[Long] with
     def apply(from : Int,v : Long)(using PreparedStatement):Int=
      stmtSetObject(from,v)
      from+1
  inline def apply()( using ()=> Connection) : ServiceImage = new Service[Image,Long] with SqlServiceImage
  

}
trait SqlServiceImage:
  self :  Service[Image,Long] =>
    def find(name : String) :Option[Image] = 
      self.findBy(ImageModel.column.name,name)
    def containsName(name : String): Boolean = 
      self.contains(ImageModel.column.name -> name)

