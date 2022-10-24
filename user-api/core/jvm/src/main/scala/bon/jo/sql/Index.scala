package bon.jo.sql

  enum IndexType:
    case Simple, Unique
  case class Index(
      var values: Seq[String] = Nil,
      indexType: IndexType = IndexType.Simple
  )