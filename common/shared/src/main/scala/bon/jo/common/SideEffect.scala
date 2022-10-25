package bon.jo.common
import typeutils.~
import scala.collection.mutable.ListBuffer
object SideEffect:
  trait Client[T]:
    def receive(t : T):Unit

  trait Serveur[T]:
    private val clients = ListBuffer.empty[Client[T]]
    def emit(t : T):Unit = 
      clients.foreach(_.receive(t))
    def register(client : Client[T]):Unit = clients += client
  def serveur[T] : ~[Serveur[T]] = summon

