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
object ScoreModel:
  case class Score(idGame : Int,lvl : Int,idUser : Long,scoreDateTime : LocalDateTime,scoreValue : Long)

  val cIdGame = "id_game"
  val cLvl = "lvl"
  val cIdUser = "id_user"
  val cScoreDate = "score_date_time"
  val cScore = "score_value"


  val scoreTable = Table{
    tableName("score")    
      Column{columnName(cIdGame);_type("INT");id} 
      Column{columnName(cLvl);_type("INT");id} 
      Column{columnName(cIdUser);_type("BIGINT");id} 
      Column{columnName(cScoreDate);_type("DATETIME")} 
      Column{columnName(cScore);_type("BIGINT")} 
    }
