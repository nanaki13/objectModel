package bon.jo.sql

enum Limit:
  case NoLimit
  case Fixed(from: Long, size: Int)
extension (l: Limit)
  def toSql: String =
    l match
      case Limit.NoLimit           => ""
      case Limit.Fixed(from, size) => s"LIMIT ${size} OFFSET ${from}"