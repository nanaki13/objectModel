package bon.jo.sql

import bon.jo.sql.CountColumns


  object BaseSqlRequest:
    inline def baseSqlRequest[T](using BaseSqlRequest[T]) = summon
    inline def table[T](using BaseSqlRequest[T]): Table = baseSqlRequest.table
    def apply[T](table: Table): BaseSqlRequest[T] = Impl[T](table)
    private case class Impl[T](table: Table) extends BaseSqlRequest[T]
  trait BaseSqlRequest[T] extends CountColumns[T]:
    val table: Table
    val count = table.columns.size
    def columnsString = table.columns.map(_.name).mkString(", ")
    def columnsStringWithoutAutoInc = table.columns
      .map(_.name)
      .filter(cName => !table.autoIncr.contains(cName))
      .mkString(", ")
    def aliasDotcolumnsString(alias: String) =
      table.columns.map(col => s"$alias.${col.name}").mkString(", ")
    def idsString = table.id.mkString(", ")
    def aliasDotidsString(alias: String) =
      table.id.map(col => s"$alias.$col").mkString(", ")
    def idsConditionString =
      s" ${table.id.map(idCol => s"$idCol = ?").mkString(" AND ")} "
    def aliasDotidsConditionString(alias: String) =
      s" ${table.id.map(idCol => s"$alias.$idCol = ?").mkString(" AND ")} "
    def columnsParamString = table.columns.map(_ => "?").mkString(", ")
    def columnsParamStringWithoutAutoInc = table.columns
      .filter(c => !table.autoIncr.contains(c.name))
      .map(_ => "?")
      .mkString(", ")
    def updateSetString: String =
      table.columns.map(c => s"${c.name} = ?").mkString(", ")
    val sqlBaseSelect = s"""SELECT $columnsString FROM ${table.name}"""
    val selectByIdString =
      s"""SELECT $columnsString FROM ${table.name} WHERE $idsConditionString"""
    val containsString = s"""SELECT 1 FROM ${table.name}"""
    val containsByIdString = s"""$containsString WHERE ${idsConditionString}"""
    val selectColumnIdIndex = table.id.zipWithIndex.toMap
    val columnIdIndex = table.columns.zipWithIndex.toMap
    val deleteByIdString =
      s"""DELETE FROM ${table.name} WHERE ${idsConditionString}"""
    val updateByIdString =
      s"""UPDATE  ${table.name} SET ${updateSetString}  WHERE $idsConditionString"""
    val insertString =
      s"""INSERT INTO ${table.name} ( $columnsStringWithoutAutoInc) VALUES ( ${columnsParamStringWithoutAutoInc} )"""
