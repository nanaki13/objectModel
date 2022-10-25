package bon.jo.sql

final case class Sort(column: String, dir: Dir)

object Sort:
  val asc: (column: String) => Sort = Sort(_, Dir.ASC)
  val desc: (column: String) => Sort = Sort(_, Dir.DESC)
enum Dir:
  case ASC, DESC
