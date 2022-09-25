package bon.jo.user

import bon.jo.sql.Sql.ResultSetMapping
import java.sql.ResultSet
import bon.jo.user.ScoreModel.Score
import java.sql.PreparedStatement
import bon.jo.sql.Sql.Service
import bon.jo.sql.Sql.ConnectionTableService
import bon.jo.sql.Sql.PSMapping
import bon.jo.sql.Sql.stmtSetObject
import java.sql.Connection
object SqlServiceScore {
  
  type ServiceScore = Service[Score,(Int,Long)] with SqlServiceScore
  
  given ResultSetMapping[Score] = r => Score(r.getInt(1),r.getLong(2),r.getObject(3).asInstanceOf,r.getLong(4))
  given ResultSetMapping[(Int,Long)]  = r =>(r.getInt(1),r.getLong(2))
  given PSMapping[Score] with
     def apply(from : Int,v : Score)(using PreparedStatement):Int=
      stmtSetObject(from,v.idGame)
      stmtSetObject(from+1,v.idUser)
      stmtSetObject(from+2,v.scoreDateTime)
      stmtSetObject(from+3,v.scoreValue)
      from+4
  given PSMapping[(Int,Long)] with
     def apply(from : Int,v : (Int,Long))(using PreparedStatement):Int=
      stmtSetObject(from,v._1)
      stmtSetObject(from+1,v._2)
      from+2
  inline def apply()( using ConnectionTableService[Score]) : ServiceScore = new Service[Score,(Int,Long)] with SqlServiceScore
  inline def apply(c : ()=> Connection): ServiceScore=apply()(using ConnectionTableService[Score](ScoreModel.scoreTable,c))

}
trait SqlServiceScore:
  self :  Service[Score,(Int,Long)] =>
   

