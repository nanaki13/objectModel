package bon.jo.common
import typeutils.~
import scala.collection.mutable.ListBuffer
object SideEffect:
  trait Client[T]:
    def receive(t : T):Unit

  trait Serveur[T]:
    println("creating serveur")
    private val clients = ListBuffer.empty[Client[T]]
    println("created clients list")
    def emit(t : T):Unit = 
      println(clients)
      clients.foreach(_.receive(t))
    def register(client : Client[T]):Unit = clients += client
    println("end creating serveur")
  def serveur[T] : ~[Serveur[T]] = summon

