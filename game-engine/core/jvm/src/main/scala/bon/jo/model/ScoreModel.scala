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
object ScoreModel:

  extension (s : Score)
    def toDomain() : bon.jo.domain.Score = bon.jo.domain.Score(s.idGame,s.lvl,s.idUser,s.scoreDateTime.toString(),s.scoreValue)

  case class Score(idGame : Int,lvl : Int,idUser : Long,scoreDateTime : LocalDateTime,scoreValue : Long)
  val cIdGame = "id_game"
  val cLvl = "lvl"
  val cIdUser = "id_user"
  val cScoreDate = "score_date_time"
  val cScore = "score_value"


  val table = Table{
    tableName("score")    
      Column{columnName(cIdGame);_type("INT");id} 
      Column{columnName(cLvl);_type("INT");id} 
      Column{columnName(cIdUser);_type("BIGINT");id} 
      Column{columnName(cScoreDate);_type("TIMESTAMP")} 
      Column{columnName(cScore);_type("BIGINT")} 
    }
