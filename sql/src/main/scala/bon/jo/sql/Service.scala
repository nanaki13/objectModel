package bon.jo.sql

import java.sql.Connection
import bon.jo.sql.ResultSetMapping
import bon.jo.sql.PSMapping
import bon.jo.sql.UsingCo
import bon.jo.sql.stmtSetObject
import bon.jo.sql.executeQuery
import bon.jo.sql.executeUpdate
import bon.jo.sql.iterator
import bon.jo.sql.SortOps.sql
import bon.jo.sql.BaseSqlRequest.baseSqlRequest
trait Service[T, ID](
    using() => Connection,
    ResultSetMapping[ID],
    ResultSetMapping[T],
    BaseSqlRequest[T],
    PSMapping[T],
    PSMapping[ID]
) extends UsingCo:

  inline def request = baseSqlRequest
  def findBy(field: String, value: Any): Option[T] =
    sql(baseSqlRequest.sqlBaseSelect + s" WHERE $field = ?") {
      stmtSetObject(1, value)
      val r = executeQuery()
      if r.next() then Some(ResultSetMapping[T](1, r))
      else None
    }
  def findBys(fieldvalue: (String, Any)*)(sorts: Seq[Sort] = Nil): Seq[T] =
    val paramsQ = fieldvalue.map(_._1).map(f => s" $f = ?").mkString(" AND ")
    sql(baseSqlRequest.sqlBaseSelect + s" WHERE $paramsQ ${sorts.sql}") {
      fieldvalue
        .map(_._2)
        .zipWithIndex
        .foreach((e, i) => stmtSetObject(i + 1, e))

      val r = executeQuery()
      r.iterator.map(r => ResultSetMapping[T](r)).toSeq
    }
  def contains(ids: ID): Boolean =
    sql(baseSqlRequest.containsByIdString) {
      PSMapping[ID](1, ids)
      val r = executeQuery()
      r.next()
    }
  def contains(fieldvalue: (String, Any)*): Boolean =
    val paramsQ = fieldvalue.map(_._1).map(f => s" $f = ?").mkString(" AND ")
    sql(baseSqlRequest.containsString + s" WHERE $paramsQ") {
      fieldvalue
        .map(_._2)
        .zipWithIndex
        .foreach((e, i) => stmtSetObject(i + 1, e))
      val r = executeQuery()
      r.next()
    }
  def maxId(): ID =
    sql[ID](
      s"SELECT MAX(${baseSqlRequest.table.id.mkString(", ")}) FROM " + baseSqlRequest.table.name
    ) {
      val r = executeQuery()
      r.next()
      ResultSetMapping[ID](r)
    }

  def read(ids: ID): T =
    sql[T](baseSqlRequest.selectByIdString) {
      PSMapping[ID](1, ids)
      val r = executeQuery()
      r.next()
      ResultSetMapping[T](r)
    }
  def readOption(ids: ID): Option[T] =
    sql(baseSqlRequest.selectByIdString) {
      PSMapping[ID](1, ids)
      val r = executeQuery()
      if r.next() then Some(ResultSetMapping[T](r))
      else None
    }
  def delete(ids: ID): Unit = sql(baseSqlRequest.deleteByIdString) {
    PSMapping[ID](1, ids)
    executeUpdate()
  }
  def update(id: ID, t: T): Unit =
    sql[T](baseSqlRequest.updateByIdString) {
      PSMapping[ID](PSMapping[T](1, t), id)
      executeUpdate()
      t
    }
  def create(t: T): T =
    sql[T](baseSqlRequest.insertString) {
      PSMapping[T].fillCreate(1, t)
      executeUpdate()
      t
    }

object Service:
  class Impl[T, ID](
      using() => Connection,
      ResultSetMapping[ID],
      ResultSetMapping[T],
      BaseSqlRequest[T],
      PSMapping[T],
      PSMapping[ID]
  ) extends Service[T, ID]
  def apply[T, ID](
      using() => Connection,
      ResultSetMapping[ID],
      ResultSetMapping[T],
      BaseSqlRequest[T],
      PSMapping[T],
      PSMapping[ID]
  ): Service[T, ID] = Impl()