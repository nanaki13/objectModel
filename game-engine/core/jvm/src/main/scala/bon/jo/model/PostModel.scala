package bon.jo.model
import bon.jo.sql.Sql.Table 
import bon.jo.sql.Sql.Column 

import bon.jo.sql.Sql.Table.id
import bon.jo.sql.Sql.Table.index
import bon.jo.sql.Sql.Table.unique
import bon.jo.sql.Sql.Table.tableName
import bon.jo.sql.Sql.Column.columnName
import bon.jo.sql.Sql.Column._type
import java.time.LocalDateTime
import bon.jo.domain.UserInfo
object PostModel:
//idSubject: Int,idUser: Long, postDateTime: String, content: String
  case class Post(idSubject: Int,idUser: Long, postDateTime: LocalDateTime, content: String)
  case class PostUser(idSubject: Int,user: UserInfo, postDateTime: LocalDateTime, content: String)
  object column:
    val idSubject = "id_subject"
    val idUser = "id_user"
    val postDateTime = "post_date_time"
    val content = "content"
  import column.*


  val table = Table{
    tableName("post")    
      Column{columnName(idSubject);_type("INT");id} 
      Column{columnName(idUser);_type("BIGINT");id} 
      Column{columnName(postDateTime);_type("TIMESTAMP");id} 
      Column{columnName(content);_type("TEXT")} 

    }
