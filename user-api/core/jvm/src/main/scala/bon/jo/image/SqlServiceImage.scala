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
import bon.jo.sql.SqlMappings.given
import bon.jo.domain.ImageInfo
object SqlServiceImage {
  
  type ServiceImage = Service[Image,Long] with SqlServiceImage
  given BaseSqlRequest[ImageInfo] = BaseSqlRequest[ImageInfo](ImageModel.imageInfoTable)
  given BaseSqlRequest[Image] = BaseSqlRequest[Image](ImageModel.imageTable)
  given ResultSetMapping[Image] with
    def apply(from : Int,r : ResultSet):Image = 
      Image(r.getLong(from),r.getString(from+1),r.getBytes(from+2))
  given ResultSetMapping[ImageInfo] with
    def apply(from : Int,r : ResultSet):ImageInfo = 
      ImageInfo(r.getLong(from),r.getString(from+1))
  given PSMapping[ImageInfo] with
    def apply(from : Int,v : ImageInfo)(using PreparedStatement):Int=
      stmtSetObject(from,v.id)
      stmtSetObject(from+1,v.name)
      from+2
  given PSMapping[Image] with
    def apply(from : Int,v : Image)(using PreparedStatement):Int=
      stmtSetObject(from,v.id)
      stmtSetObject(from+1,v.name)
      stmtSetObject(from+2,v.data)
      from+3
    override def fillCreate(from : Int,v : Image)(using PreparedStatement):Int=
      stmtSetObject(from,v.name)
      stmtSetObject(from+1,v.data)
      from+2

  inline def apply()( using ()=> Connection) : ServiceImage = new Service[Image,Long] with SqlServiceImage
  

}
trait SqlServiceImage:
  self :  Service[Image,Long] =>
    def find(name : String) :Option[Image] = 
      self.findBy(ImageModel.column.name,name)
    def containsName(name : String): Boolean = 
      self.contains(ImageModel.column.name -> name)

