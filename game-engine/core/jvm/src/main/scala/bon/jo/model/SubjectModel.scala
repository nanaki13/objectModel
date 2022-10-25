package bon.jo.model
import bon.jo.sql.Table 
import bon.jo.sql.Column 

import bon.jo.sql.Table.id
import bon.jo.sql.Table.index
import bon.jo.sql.Table.unique
import bon.jo.sql.Table.tableName
import bon.jo.sql.Column.columnName
import bon.jo.sql.Column._type
import java.time.LocalDateTime
import bon.jo.domain.UserInfo
object SubjectModel:
//idSubject: Int,idUser: Long, postDateTime: String, content: String
  case class Subject(id: Int, subjectDateTime: LocalDateTime, title: String, description: String)
  object column:
    val id = "id"
    val subjectDateTime = "subject_date_time"
    val title = "title"
    val description = "description"
  import column as c


  val table = Table{
    tableName("subject")    
      Column{columnName(c.id);_type("INT");id} 
      Column{columnName(c.subjectDateTime);_type("TIMESTAMP")} 
      Column{columnName(c.title);_type("VARCHAR(150)")} 
      Column{columnName(c.description);_type("TEXT")} 

    }
