package bon.jo.sql

import java.sql.PreparedStatement
import bon.jo.sql.SortOps.sql
import bon.jo.common.Anys.toSome
trait SelectOps:
  sel : UsingCo =>
    lazy val sqlBaseSelect : String
    type Ret 
    def bindToStmt(fieldvalue: Seq[(String, Any)]): PreparedStatement ?=> Unit =
      fieldvalue
        .map(_._2)
        .zipWithIndex
        .foreach((e, i) => stmtSetObject(i + 1, e))
    def paramsQ(fieldvalue: Seq[(String, Any)]) =
      fieldvalue.map(_._1).map(f => s" $f = ?").mkString(" AND ")
    def select(fieldvalue: Seq[(String, Any)]) =
      sqlBaseSelect + s" WHERE ${paramsQ(fieldvalue)}"
    def findAllBys(fieldvalue: (String, Any)*)(
        sorts: Seq[Sort] = Nil,
        limit: Limit = Limit.NoLimit
    ): ResultSetMapping[Ret] ?=> Seq[Ret] =

      val sqlS = select(fieldvalue) + sorts.sql + " " + limit.toSql
      sql(sqlS) {
        bindToStmt(fieldvalue)
        val r = executeQuery()
        r.iterator
          .map(r => {

            ResultSetMapping[Ret](1, r)
          })
          .toSeq
      }
    def findBys(
        fieldvalue: (String, Any)*
    ): ResultSetMapping[Ret] ?=> Option[Ret] =

      val sqlS = select(fieldvalue)
      println(sqlS)
      sql(sqlS) {
        bindToStmt(fieldvalue)
        val r = executeQuery()
        if r.next() then ResultSetMapping[Ret](1, r).toSome else None

      }