package bon.jo.user
import bon.jo.sql.Sql.Table 
import bon.jo.sql.Sql.Column 

import bon.jo.sql.Sql.Table.id
import bon.jo.sql.Sql.Table.index
import bon.jo.sql.Sql.Table.unique
import bon.jo.sql.Sql.Table.tableName
import bon.jo.sql.Sql.Column.columnName
import bon.jo.sql.Sql.Column._type
import java.time.LocalDateTime
object ScoreModel:
  case class Score(idGame : Int,idUser : Long,scoreDateTime : LocalDateTime,scoreValue : Long)



  val scoreTable = Table{
    tableName("score")    
      Column{columnName("id_game");_type("INT");id} 
      Column{columnName("id_user");_type("BIGINT");id} 
      Column{columnName("score_date_time");_type("DATETIME")} 
      Column{columnName("score_value");_type("BIGINT")} 
    }
