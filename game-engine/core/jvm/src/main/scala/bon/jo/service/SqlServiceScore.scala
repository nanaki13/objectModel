package bon.jo.service

import bon.jo.sql.Sql.ResultSetMapping
import java.sql.ResultSet
import bon.jo.model.ScoreModel.Score
import bon.jo.model.ScoreModel
import java.sql.PreparedStatement
import bon.jo.sql.Sql.Service
import bon.jo.sql.Sql.ConnectionTableService
import bon.jo.sql.Sql.PSMapping
import bon.jo.sql.Sql.stmtSetObject
import bon.jo.sql.Sql.stmt
import java.sql.Connection
import java.time.LocalDateTime
object SqlServiceScore {
  
  type ServiceScore = Service[Score,(Int,Int,Long)] with SqlServiceScore
  def toLocalDateTime(a : Any):LocalDateTime = 
    a match
      case e : LocalDateTime => e
      case o =>  LocalDateTime.parse(o.toString())
   
  given ResultSetMapping[Score] = 
    (from,r) => 
      Score(r.getInt(from),r.getInt(from+1),r.getLong(from+2),toLocalDateTime(r.getObject(from+3)),r.getLong(from+4))
  given ResultSetMapping[(Int,Int,Long)]  = (from,r) =>(r.getInt(from),r.getInt(from +1 ),r.getLong(from+2))
  given PSMapping[Score] with
     def apply(from : Int,v : Score)(using PreparedStatement):Int=
      stmtSetObject(from,v.idGame)
      stmtSetObject(from+1,v.lvl)
      stmtSetObject(from+2,v.idUser)
      stmtSetObject(from+3,v.scoreDateTime)
      stmtSetObject(from+4,v.scoreValue)
      from+5
  given PSMapping[(Int,Int,Long)] with
     def apply(from : Int,v : (Int,Int,Long))(using PreparedStatement):Int=
      stmtSetObject(from,v._1)
      stmtSetObject(from+1,v._2)
      stmtSetObject(from+2,v._3)
      println(stmt)
      from+3
  inline def apply()( using ConnectionTableService[Score]) : ServiceScore = new Service[Score,(Int,Int,Long)] with SqlServiceScore
  inline def apply(c : ()=> Connection): ServiceScore=apply()(using ConnectionTableService[Score](ScoreModel.scoreTable,c))

}
trait SqlServiceScore:
  self :  Service[Score,(Int,Int,Long)] =>
    def readScore(gameId : Int,level:Int) : Seq[Score] = findBys((ScoreModel.cIdGame,gameId),(ScoreModel.cLvl,level))
    def readScore(gameId : Int,level:Int, userId : Long) : Option[Score] = readOption(gameId, level, userId )
    inline def readScore(score :  Score) : Option[Score] = readScore(score.idGame,score.lvl,score.idUser)
    def updateScore(score : Score):Boolean =
      def createF() : Boolean = 
        create(score)
        true  
      readScore(score ).map(_.scoreValue) match
        case Some(scorePrevious) if scorePrevious < score.scoreValue  => createF()         
        case None =>  createF()
        case _ => false

   

