package bon.jo

package sql:

  import java.sql.ResultSet
 
  extension (r : ResultSet)
    def iterator:Iterator[ResultSet] = new :
      def hasNext():Boolean = r.next()
      override def next():ResultSet = r
    def to[T](using Conversion[ResultSet,T]):Iterator[T] = 
      r.iterator.map(_.convert)
    def toRawList = 
      import ToSeq.given
      r.to[Seq[Any]]
    def toRawNamedList = 
      import ToNamedSeq.given
      r.to[Seq[(String,Any)]]
  object ToSeq:
    given Conversion[ResultSet,Seq[Any]] =
      e => 
        for(i <- 1 to e.getMetaData().getColumnCount()) yield e.getObject(i)
  object ToNamedSeq:
    given Conversion[ResultSet,Seq[(String,Any)]] =
      e => 
        for(i <- 1 to e.getMetaData().getColumnCount()) yield e.getMetaData().getColumnName(i) -> e.getObject(i)
