package bon.jo.sql

object SortOps:
    extension (sorts: Seq[Sort])
      def sql: String =
        if sorts.isEmpty then ""
        else " ORDER BY " + sorts.map(e => e.column + " " + e.dir).mkString(",")
