package bon.jo.sql

import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet

object ScanDB:
  given Conversion[ResultSet,(String, model.Table)] = 
    r => r.getString("TABLE_SCHEM") -> model.Table(r.getString("TABLE_NAME"),r.getString("TABLE_TYPE"),Nil,Nil)
  given Conversion[ResultSet,model.Schema] = 
    r => model.Schema(r.getString("SCHEMA_NAME"),Nil)
  given Conversion[ResultSet,(String,String,model.Column)] = 
    r => (r.getString("TABLE_SCHEM"),r.getString("TABLE_NAME"), model.Column(r.getString("COLUMN_NAME"),r.getString("TYPE_NAME")))
  def apply(e : Connection):model.DB = 
    val schemas = e.getMetaData().getSchemas().toRawNamedList
    val tables = e.getMetaData().getTables(null,null,null,null).toRawNamedList
    val columns = e.getMetaData().getColumns(null,null,null,null).toRawNamedList
    
    schemas.foreach(println)
    println("=" * 100)
    tables.foreach(println)
    println("=" * 100)
    columns.foreach(println)
    println("=" * 100)
   
    var schemas_ = e.getMetaData().getSchemas().to[model.Schema].toSeq
    var tables_ = e.getMetaData().getTables(null,null,null,null).to[(String, model.Table)].toSeq
    var columns_ = e.getMetaData().getColumns(null,null,null,null).to[(String,String,model.Column)].toSeq
    schemas_.foreach(println)
    tables_.foreach(println)
    columns_.foreach(println)
    
    val pks = tables_.filter(_._2.tableType == "TABLE").map{
      t => {
        val fks= e.getMetaData().getExportedKeys(null,null,t._2.name).toRawNamedList.toSeq
        println("=" * 100)
        fks.foreach(println)
        println("=" * 100)
        t._1 -> t._2.copy(pks =e.getMetaData().getPrimaryKeys(null,t._1,t._2.name).toRawNamedList.toSeq.map(_.find(_._1 == "COLUMN_NAME").get).map(_._2.toString()) ) 
      }
    }
    tables_ = tables_.filter(_._2.tableType != "TABLE") ++ pks
    pks.foreach(println)
    println("=" * 100)
    val (schemas__,tables__,columns__) = if schemas_.isEmpty then
      (Seq(model.Schema("root",Nil)),tables_.map((sc,t) => if sc == null then "root"-> t else sc -> t),columns_.map((sc,t,c) => if sc == null then ("root", t ,c)else (sc, t ,c)))
    else
      (schemas_,tables_,columns_)

    val tablesMap = tables__.groupBy(_._1).mapValues(_.map(_._2)).toMap
    val columnsMap =  columns__.groupBy(_._1).mapValues(_.map((_,t,c)=> t->c).groupBy(_._1).mapValues(_.map(_._2).toSeq).toMap).toMap

    val fullSchema =schemas__.map{
      s => s.copy(tables = {
        tablesMap(s.name).map{
          t => t.copy(columns = columnsMap(s.name).getOrElse(t.name,Nil))
        }
      })
    }

    println("=" * 100)
    model.DB("db",fullSchema)

  @main
  def test():Unit = 
    val con = DriverManager.getConnection("""jdbc:sqlite:C:\Users\Jonathan\Desktop\objectModel\game-engine\sample2.db""")
    println("=" * 100)
    println(ScanDB(con))
    con.close()

  