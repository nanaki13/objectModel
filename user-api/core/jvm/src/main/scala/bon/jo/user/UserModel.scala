package bon.jo.user
import bon.jo.sql.Sql.Table 
import bon.jo.sql.Sql.Column 

import bon.jo.sql.Sql.Table.id
import bon.jo.sql.Sql.Table.tableName
import bon.jo.sql.Sql.Column.columnName
import bon.jo.sql.Sql.Column._type
object UserModel:
  case class User(id : Long,name : String,pwd : String)
  case class UserLogin(name : String,pwd : String)
  case class UserInfo(id : Long,name : String)
  extension (e : User)
    def toUserInfo = UserInfo(e.id,e.name)
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
        _type("VARCHAR(255)")
    }
    Column{
        columnName(cPwd)
        _type("VARCHAR(255)")
    }
      
    
  }