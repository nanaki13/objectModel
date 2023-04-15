package bon.jo.sql

package model:
  case class Column(name : String, _type : String)
  case class Table(name : String, tableType : String, columns : Seq[Column], pks : Seq[String])
  case class Schema(name : String, tables : Seq[Table])
  case class DB(name : String, schemas:Seq[Schema])
