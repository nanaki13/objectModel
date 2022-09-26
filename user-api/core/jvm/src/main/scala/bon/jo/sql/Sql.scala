package bon.jo.sql

import java.sql.Connection
import bon.jo.sql.StringWriter.*
import java.sql.PreparedStatement
import java.sql.ResultSet
import bon.jo.sql.Sql.PSMapping

import bon.jo.sql.Sql.ConnectionTableService
import ConnectionTableService.connectionTableService
import java.sql.Statement
object Sql {
  case class Builder(var value : Table,val id : ColumnsRef = ColumnsRef(),var indexs : Seq[Index] = Nil)
  case class BuilderCol(var value : Column)
  type ColumnInTable = Builder ?=> Column
  type OnBuildColumnInTable = (Builder,BuilderCol) ?=> Unit
  type OnBuild = Builder ?=> Unit
  type OnBuildCol = BuilderCol ?=> Unit
  inline def tableBuilder: Builder ?=> Builder = summon
  inline def columnBuilder: BuilderCol ?=> BuilderCol = summon
  inline def tableid: Builder ?=> ColumnsRef = tableBuilder.id
  extension (r : ResultSet)
    def iterator : Iterator[ResultSet] = new Iterator[ResultSet]{
        def hasNext: Boolean = r.next()
        def next():ResultSet =r
    }
  class ColumnsRef(var values : Seq[String] = Nil)
  enum IndexType:
    case Simple,Unique
  case class Index(var values : Seq[String] = Nil,indexType : IndexType = IndexType.Simple)

  object Table:
    def tableName(name : String): OnBuild = 
      summon.value = summon.value.copy(name)  
    def id: OnBuildColumnInTable = 
      tableid.values = tableid.values :+ columnBuilder.value.name   
    def index: OnBuildColumnInTable = 
      tableBuilder.indexs = tableBuilder.indexs  :+ Index(columnBuilder.value.name :: Nil)
    def unique: OnBuildColumnInTable = 
      tableBuilder.indexs = tableBuilder.indexs  :+ Index(columnBuilder.value.name :: Nil, IndexType.Unique)

    def apply(f : OnBuild ):Table = 
      given Builder = Builder(Table("",Nil , Nil, Nil))
      f

      summon.value.copy(id = tableBuilder.id.values,indexs =  tableBuilder.indexs)
    def col(f : OnBuildCol ): OnBuild = 
      given BuilderCol = BuilderCol(Column("",""))
      f
      summon.value
  case class Table(name : String,columns : Seq[Column],id:Seq[String],indexs:Seq[Index]):
    def createSql : String = 
      s"""CREATE TABLE $name(
        ${columns.mkString(",\n        ")},
        PRIMARY KEY(${id.mkString(",")}));
        ${indexs.map{
          index => 
            val indexType = index.indexType match
              case IndexType.Unique => "UNIQUE"
              case _ => ""
            
            s"""CREATE $indexType INDEX ${index.values.mkString("_")}_idx ON $name(${index.values.mkString(",")});"""
        }.mkString("\n")}"""
  case class Column(name : String,dbType : String):
    override def toString() = s"$name $dbType"
  object Column:
    def columnName(name : String): OnBuildCol = 
      summon.value = summon.value.copy(name)  
    def _type(name : String): OnBuildCol = 
      summon.value = summon.value.copy(dbType = name)  
    def apply(f : OnBuildCol * ):ColumnInTable = 
      given BuilderCol = BuilderCol(Column("",""))
      f.foreach(ff => ff)
      val t = tableBuilder.value
      val c =  columnBuilder.value
      tableBuilder.value =  t.copy(columns = t.columns :+  c) 
      c
  inline def prepare(sql :String)(using Connection):PreparedStatement = summon.prepareStatement(sql)

  inline def stmtSetObject(i : Int,o : Any)(using PreparedStatement) =  summon.setObject(i,o)
  inline def executeQuery()(using PreparedStatement): ResultSet =  summon.executeQuery()
  inline def executeUpdate()(using PreparedStatement): Int =  summon.executeUpdate()
  inline def execute()(using PreparedStatement): Boolean =  summon.execute()
  inline def stmtClose(using Statement) = summon.close
 
  def doSql[A](sql : String)(f : PreparedStatement ?=> A)(using Connection) :A = 
  
    given PreparedStatement = prepare(sql)
    val rest = f
    stmtClose
    rest
  inline def stmt : Statement ?=> Statement = summon
  def stmtDo[A]()(f : Statement ?=> A)(using c :Connection) :A = 
    given Statement = c.createStatement()
    val rest = f
    stmtClose
    rest
 

  trait ResultSetMapping[T]:
    def apply(from : Int,v : ResultSet):T
    inline def apply(v : ResultSet):T = this(1,v)
  object ResultSetMapping:
    inline def apply[T](using ResultSetMapping[T]) = summon
  trait PSMapping[T]:
    def apply(from : Int,v : T)(using PreparedStatement):Int
  object PSMapping:
    inline def apply[T](using PSMapping[T]) = summon

  
  inline def mapping[T] :  ResultSetMapping[T] ?=>  ResultSetMapping[T] = summon


    
  object ConnectionTableService:
    inline def connectionTableService[T](using ConnectionTableService[T]) = summon
    def apply[T](table : Table,c : ()=> Connection):ConnectionTableService[T] = Impl[T](table,c)
    private case class Impl[T](table : Table,c : ()=> Connection) extends ConnectionTableService[T]:
      def connection() : Connection = c()
  trait ConnectionTableService[T] :
    val table : Table
    def connection() : Connection
    def columnsString = table.columns.map(_.name).mkString(", ")
    def idsString = table.id.mkString(", ")
    def idsConditionString = s" ${table.id.map(idCol => s"$idCol = ?").mkString(" AND ")} "
    def columnsParamString = table.columns.map(_ => "?").mkString(", ")
    def updateSetString : String = table.columns.map(c => s"${c.name} = ?").mkString(", ")
    val sqlBaseSelect = s"""SELECT $columnsString FROM ${table.name}"""
    val selectByIdString = s"""SELECT $columnsString FROM ${table.name} WHERE $idsConditionString"""
    val containsByIdString = s"""SELECT 1 FROM ${table.name} WHERE ${idsConditionString}"""
    val selectColumnIdIndex= table.id.zipWithIndex.toMap
    val columnIdIndex= table.columns.zipWithIndex.toMap
    val deleteByIdString = s"""DELETE FROM ${table.name} WHERE ${idsConditionString}"""
    val updateByIdString = s"""UPDATE  ${table.name} SET ${updateSetString}  WHERE $idsConditionString"""
    val insertString = s"""INSERT INTO ${table.name} ( $columnsString) VALUES ( ${columnsParamString} )"""
    def sql[A](sql : String)(f : PreparedStatement ?=> A):A = doSql(sql)(f)(using connection())


      
  trait Service[T,ID](using ResultSetMapping[ID],ResultSetMapping[T],ConnectionTableService[T],PSMapping[T],PSMapping[ID]):
  
    inline def service = connectionTableService

    def findBy(field : String,value : Any):Option[T] = 
       service.sql(service.sqlBaseSelect+s" WHERE $field = ?"){
        stmtSetObject(1,value)
        val r = executeQuery()
        if r.next() then
          Some(ResultSetMapping[T](1,r))
        else 
          None
      } 
    def findBys(fieldvalue : (String,Any) *):Seq[T] = 
      val paramsQ = fieldvalue.map(_._1).map(f => s" $f = ?").mkString(" AND ")
      service.sql(service.sqlBaseSelect+s" WHERE $paramsQ"){
        fieldvalue.map(_._2).zipWithIndex.foreach((e,i) => stmtSetObject(i+1,e))
        
        val r = executeQuery()
        r.iterator.map( r => ResultSetMapping[T](r)).toSeq
      } 
    def contains(ids : ID):Boolean = 
       service.sql(service.selectByIdString){
        PSMapping[ID](1,ids)
        val r = executeQuery()
        r.next()
      }
    def maxId():ID = 
      service.sql[ID](s"SELECT MAX(${service.table.id.mkString(", ")}) FROM "+service.table.name){
        val r = executeQuery()
        r.next()
        ResultSetMapping[ID](r)
      }
    def read(ids :ID):T  = 
      service.sql[T](service.selectByIdString){
        PSMapping[ID](1,ids)
        val r = executeQuery()
        r.next()
        ResultSetMapping[T](r)
      }
    def readOption(ids :ID):Option[T]  = 
      service.sql(service.selectByIdString){
        PSMapping[ID](1,ids)
        val r = executeQuery()
        if r.next() then
          Some(ResultSetMapping[T](r))
        else 
          None
      }
    def delete(ids :ID):Unit = service.sql(service.deleteByIdString){
        PSMapping[ID](1,ids)
        executeUpdate()     
      }
    def update(id : ID,t : T):Unit= 
      service.sql[T](service.updateByIdString){
        PSMapping[ID](PSMapping[T](1,t),id)
        executeUpdate()
        t
      } 
    def create(t :T):T= 
      service.sql[T](service.insertString){
        PSMapping[T](1,t)
        executeUpdate()
        t
      } 

  object Service:
    class Impl[T,ID](using ResultSetMapping[ID], ResultSetMapping[T],ConnectionTableService[T],PSMapping[T],PSMapping[ID]) extends Service[T,ID]
    def apply[T,ID](using ResultSetMapping[ID],ResultSetMapping[T],ConnectionTableService[T],PSMapping[T],PSMapping[ID]): Service[T,ID] = Impl()
  
}
