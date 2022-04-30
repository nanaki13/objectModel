package bon.jo.user
import bon.jo.sql.Sql.Table 
import bon.jo.sql.Sql.Column 

import bon.jo.sql.Sql.Table.id
import bon.jo.sql.Sql.Table.tableName
import bon.jo.sql.Sql.Column.columnName
import bon.jo.sql.Sql.Column._type
object UserModel:
  case class User(id : Long,name : String,pwd : String)

  val cId = "id"
  val cName = "name"
  val cPwd = "pwd"

  val userTable = Table{
    tableName("user")
    Column{
      columnName("id")
      _type("INTEGER")
      id()
    }   
    Column{
        columnName(cName)
        _type("TEXT")
    }
    Column{
        columnName(cPwd)
        _type("TEXT")
    }
      
    
  }