package bon.jo.image
import bon.jo.sql.Table 
import bon.jo.sql.Column  

import bon.jo.sql.Table.id
import bon.jo.sql.Table.index
import bon.jo.sql.Table.unique
import bon.jo.sql.Table.autoIncr
import bon.jo.sql.Table.tableName
import bon.jo.sql.Column.columnName
import bon.jo.sql.Column._type
import bon.jo.domain.{ Image}
object ImageModel:
  //  case class Image(id : Long, name : String, data : Array[Byte])
  object column:
    val id = "id"
    val name = "name"
    val data = "data"

  val imageTable = Table{
    tableName("image")
      Column{columnName(column.id);_type("BIGINT");id;autoIncr} 
      Column{columnName(column.name);_type("VARCHAR(255)");unique} 
      Column{columnName(column.data);_type("BLOB")} 


    }
  val imageInfoTable = imageTable.copy(columns = imageTable.columns.filter(_.name != column.data))
