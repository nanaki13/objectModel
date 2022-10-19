package bon.jo.common

import scala.concurrent.Future

object Anys:
  extension [T] (t : T)
    def toSome : Some[T] = Some(t)
    def toFuture: Future[T] = Future.successful(t)
    def toSeq : Seq[T] = toSome.toSeq
  extension [T] (t : Option[T])
    def toFuture: Future[T] = 
      t match
        case None => Future.failed(new IllegalStateException("no value"))
        case Some(value) =>Future.successful(value)
      
