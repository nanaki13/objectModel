package bon.jo.sql
import java.sql.Connection
enum DBType:
  case SqLite,Postgre

object DBType:
  given (using c : Connection): DBType =
    val dbName = c.getMetaData().getDatabaseProductName()
    dbName match

      case _ => throw MatchError("cant find : "+dbName)
    
