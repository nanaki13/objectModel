package bon.jo.user
import bon.jo.sql.Sql.Table 
import bon.jo.sql.Sql.Column 

import bon.jo.sql.Sql.Table.id
import bon.jo.sql.Sql.Table.index
import bon.jo.sql.Sql.Table.unique
import bon.jo.sql.Sql.Table.tableName
import bon.jo.sql.Sql.Column.columnName
import bon.jo.sql.Sql.Column._type
import bon.jo.domain.{UserInfo , User, UserAvatar}
import bon.jo.domain.given
object UserModel:

  extension (e : UserAvatar)
    def toUserInfo = UserInfo(e.user.id,e.user.name,e.avatar)
  object column:
    val id = "id"
    val name = "name"
    val pwd = "pwd"
    val avatarKey = "avatar_image_key"

  val userTable = Table{
    tableName("users")
      Column{columnName(column.id);_type("BIGINT");id} 
      Column{columnName(column.name);_type("VARCHAR(255)");unique} 
      Column{columnName(column.pwd);_type("VARCHAR(255)")}
      Column{columnName(column.avatarKey);_type("BIGINT")}


    }
  val userInfoTable = userTable.copy(columns = userTable.columns.filter(_.name != column.pwd))
