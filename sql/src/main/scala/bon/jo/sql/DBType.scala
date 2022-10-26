package bon.jo.sql
import java.sql.Connection
enum DBType:
  case SQLite,PostgreSQL

object DBType:
  given (using c : Connection): DBType =
    val dbName = c.getMetaData().getDatabaseProductName()
    DBType.valueOf(dbName)
    
