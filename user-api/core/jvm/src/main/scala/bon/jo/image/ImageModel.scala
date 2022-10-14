package bon.jo.image
import bon.jo.sql.Sql.Table 
import bon.jo.sql.Sql.Column 

import bon.jo.sql.Sql.Table.id
import bon.jo.sql.Sql.Table.index
import bon.jo.sql.Sql.Table.unique
import bon.jo.sql.Sql.Table.autoIncr
import bon.jo.sql.Sql.Table.tableName
import bon.jo.sql.Sql.Column.columnName
import bon.jo.sql.Sql.Column._type
import bon.jo.domain.{ Image}
object ImageModel:
  //  case class Image(id : Long, name : String, data : Array[Byte])
  object column:
    val id = "id"
    val name = "name"
    val data = "data"

  val userTable = Table{
    tableName("image")
      Column{columnName(column.id);_type("BIGINT");id;autoIncr} 
      Column{columnName(column.name);_type("VARCHAR(255)");unique} 
      Column{columnName(column.data);_type("BLOB")} 


    }
