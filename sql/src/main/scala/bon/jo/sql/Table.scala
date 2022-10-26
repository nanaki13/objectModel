package bon.jo.sql
import java.sql.ResultSet


  object Table:
    case class BuilderTable(
      var value: Table,
      val id: ColumnsRef = ColumnsRef(),
      var indexs: Seq[Index] = Nil,
      var autoInc: Seq[String] = Nil
    )
    case class BuilderCol(var value: Column)
    type ColumnInTable = BuilderTable ?=> Column
    type OnBuildColumnInTable = (BuilderTable, BuilderCol) ?=> Unit
    type OnBuild = BuilderTable ?=> Unit
    type OnBuildCol = BuilderCol ?=> Unit
    inline def tableBuilder: BuilderTable ?=> BuilderTable = summon
    inline def columnBuilder: BuilderCol ?=> BuilderCol = summon
    inline def tableid: BuilderTable ?=> ColumnsRef = tableBuilder.id

    class ColumnsRef(var values: Seq[String] = Nil)
    def tableName(name: String): OnBuild =
      summon.value = summon.value.copy(name)
    def id: OnBuildColumnInTable =
      tableid.values = tableid.values :+ columnBuilder.value.name
    def autoIncr: OnBuildColumnInTable =
      tableBuilder.autoInc = tableBuilder.autoInc :+ columnBuilder.value.name
    def index: OnBuildColumnInTable =
      tableBuilder.indexs =
        tableBuilder.indexs :+ Index(columnBuilder.value.name :: Nil)
    def unique: OnBuildColumnInTable =
      tableBuilder.indexs = tableBuilder.indexs :+ Index(
        columnBuilder.value.name :: Nil,
        IndexType.Unique
      )

    def apply(f: OnBuild): Table =
      given BuilderTable = BuilderTable(Table("", Nil, Nil, Nil, Set.empty))
      f

      summon.value.copy(
        id = tableBuilder.id.values,
        indexs = tableBuilder.indexs,
        autoIncr = tableBuilder.autoInc.toSet
      )
    def col(f: OnBuildCol): OnBuild =
      given BuilderCol = BuilderCol(Column("", ""))
      f
      summon.value
  case class Table(
      name: String,
      columns: Seq[Column],
      id: Seq[String],
      indexs: Seq[Index],
      autoIncr: Set[String]
  ):
    def createSql_ : String =
      s"""CREATE TABLE $name(
        ${columns.mkString(",\n        ")},
        PRIMARY KEY(${id.mkString(",")}));
        ${indexs
          .map { index =>
            val indexType = index.indexType match
              case IndexType.Unique => "UNIQUE"
              case _                => ""

            s"""CREATE $indexType INDEX ${index.values.mkString(
                "_"
              )}_idx ON $name(${index.values.mkString(",")});"""
          }
          .mkString("\n")}"""
    def createSql(using dbTypeOf: DBType): String =
      given Set[String] = autoIncr
      s"""CREATE TABLE $name(
        ${columns.map(_.sqlDef).mkString(",\n        ")}
        ${
          if dbTypeOf == DBType.SQLite && autoIncr.nonEmpty then ""
          else s""",PRIMARY KEY(${id.mkString(",")})"""
        }
        );
        ${indexs
          .map { index =>
            val indexType = index.indexType match
              case IndexType.Unique => "UNIQUE"
              case _                => ""

            s"""CREATE $indexType INDEX ${name}_${index.values.mkString(
                "_"
              )}_idx ON $name(${index.values.mkString(",")});"""
          }
          .mkString("\n")}"""
