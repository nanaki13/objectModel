package bon.jo.sql

import java.sql.Connection
import bon.jo.sql.StringWriter.*
import java.sql.PreparedStatement
import java.sql.ResultSet
import bon.jo.sql.PSMapping
import bon.jo.common.typeutils.~
import bon.jo.common.Anys.toSome
import BaseSqlRequest.baseSqlRequest
import java.sql.Statement

import SortOps.*






extension (r: ResultSet)
  def iterator: Iterator[ResultSet] = new Iterator[ResultSet] {
    def hasNext: Boolean = r.next()
    def next(): ResultSet = r
  }

inline def prepare(sql: String)(using Connection): PreparedStatement =
  summon.prepareStatement(sql)

inline def stmtSetObject(i: Int, o: Any)(using PreparedStatement) =
  summon.setObject(i, o)
inline def executeQuery()(using PreparedStatement): ResultSet =
  summon.executeQuery()
inline def executeUpdate()(using PreparedStatement): Int =
  summon.executeUpdate()
inline def execute()(using PreparedStatement): Boolean = summon.execute()
inline def stmtClose(using Statement) = summon.close

def doSql[A](sql: String)(f: PreparedStatement ?=> A)(using Connection): A =

  given PreparedStatement = prepare(sql)
  val rest = f
  stmtClose
  rest
inline def stmt: Statement ?=> Statement = summon
def stmtDo[A]()(f: Statement ?=> A)(using c: Connection): A =
  given Statement = c.createStatement()
  val rest = f
  stmtClose
  rest

trait CountColumns[T]:
  def count: Int
object CountColumns:
  inline def apply[T](using CountColumns[T]) = summon
  given [L](using Join2TableRequest[L, _, _]): CountColumns[L] =
    summon.leftTable
trait ResultSetMapping[T]:
  def apply(from: Int, v: ResultSet): T
  inline def apply(v: ResultSet): T = this(1, v)
object ResultSetMapping:
  inline def apply[T](using ResultSetMapping[T]) = summon
  
trait PSMapping[T]:
  def apply(from: Int, v: T)(using PreparedStatement): Int
  def fillCreate(from: Int, v: T)(using PreparedStatement): Int =
    apply(from, v)
object PSMapping:
  inline def apply[T](using PSMapping[T]) = summon

inline def mapping[T]: ResultSetMapping[T] ?=> ResultSetMapping[T] = summon




type RetC[J1[_, _], T1, T2] = J1[T1, T2] match
  case JoinType.Default[T1, T2] => (T1, T2)
  case JoinType.Left[T1, T2]    => (T1, Option[T2])
  case JoinType.Right[T1, T2]   => (Option[T1], T2)


trait UsingCo(using() => Connection):
  def connection: () => Connection = summon
  def sql[A](sql: String)(f: PreparedStatement ?=> A): A =
    doSql(sql)(f)(using summon[() => Connection]())








