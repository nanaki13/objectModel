package bon.jo.sql
import Table.*

 case class Column(name: String, dbType: String):
  override def toString() = s"$name $dbType"
  inline def isAi(using ai: Set[String]) = ai.contains(name)
  def sqlDef(using ai: Set[String], dbTypeOf: DBType): String =

    val dBDependType =   dbTypeOf match
        case DBType.PostgreSQL if isAi =>
          val aiType = dbType match
            case "INT"    => "SERIAL"
            case "BIGINT" => "BIGSERIAL"
          s"$aiType"

        case DBType.SQLite if isAi  => "INTEGER PRIMARY KEY AUTOINCREMENT"
        case  DBType.PostgreSQL if dbType == "BLOB" => "bytea"
        case _ => dbType
    s"$name $dBDependType"
     

object Column:
  def columnName(name: String): OnBuildCol =
    summon.value = summon.value.copy(name)
  def _type(name: String): OnBuildCol =
    summon.value = summon.value.copy(dbType = name)
  def apply(f: OnBuildCol*): ColumnInTable =
    given BuilderCol = BuilderCol(Column("", ""))
    f.foreach(ff => ff)
    val t = tableBuilder.value
    val c = columnBuilder.value
    tableBuilder.value = t.copy(columns = t.columns :+ c)
    c
