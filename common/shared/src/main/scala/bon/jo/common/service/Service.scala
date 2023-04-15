package bon.jo.common.service

import scala.concurrent.Future

import concurrent.ExecutionContext.Implicits.global

trait Service[T,Id] :
  type C[A]
  type Query[A]
  def get(id : Id):C[T]
  def set(id : Id,t : T):C[T]
  def query(q : Query[T]):C[Seq[T]]

object Service:

  
  trait AsynService[T,Id] extends Delegate[T,Id]:
    type C[A] = Future[A]
    val delegate : SynService[T,Id]
      
    type Query[A] = delegate.Query[A]
    def get(id : Id):C[T] = Future(delegate.get(id))
    def set(id : Id,t : T):C[T] = Future(delegate.set(id,t))
    def query(q : Query[T]):C[Seq[T]] = Future(delegate.query(q))
  trait SynService[T,Id] extends Service[T,Id]:
    type C[A] = A
  trait Delegate[T,Id] extends Service[T,Id]:
    val delegate : Service[T,Id]

  trait MappedDelegate[T,Id] extends Delegate[T,Id]:
    given convC[A] : Conversion[delegate.C[A],C[A]]
    given convQuery[A] : Conversion[Query[A],delegate.Query[A]]
    def get(id : Id):C[T] = delegate.get(id).convert
    def set(id : Id,t : T):C[T] = delegate.set(id,t).convert
    def query(q : Query[T]):C[Seq[T]] = delegate.query(q.convert).convert


