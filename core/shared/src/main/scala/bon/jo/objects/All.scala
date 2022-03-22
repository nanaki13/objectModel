package bon.jo.objects
import All.*

import java.time.LocalDate
import scala.collection.immutable
object All:
  case class ObjectProp[K,V <: All[K]](key : K, value: V)
  type L[A] = scala.collection.immutable.List[A]
  val L: immutable.List.type = scala.collection.immutable.List
  type Prop[Key] = ObjectProp[Key,All[Key]]

  //@main
  def main() =
    type Obj = All[String]
    import Dsl.*
    val objee : Obj= obj{
      "name" := "bob"
      "groupe" := obj{
        "name" := "best"
      }
      "birthDate" := LocalDate.of(2000,1,1)
    }
    println(objee / "groupe"  / "name")

  case class Path[K](values : L[K]):
    def /(s : K): Path[K] = Path(values :+ s)
  object Dsl:

    extension [K](s : K)
      def :=(v : All[K]):  OnBuild[K] =
        buff.value =  buff.value.copy(buff.value.props.filter(_.key != s) :+ ObjectProp(s,v))
        buff

      def :=[V](v : V):  OnBuild[K] =
        s := All.Value[K,V](v)
        buff

    case class Buff[A](var value : All.Object[A])
    type Flow[K,A] = Buff[K] ?=> A
    type OnBuild[K] = Flow[K,Buff[K]]
    def buff[K] : OnBuild[K] = summon
    def buildValue[K] : Flow[K,All.Object[K]] = buff.value


    def obj[K](build : OnBuild[K]):All.Object[K] =
      given  Buff[K] = Buff[K](All.Object(Nil))
      build
      buildValue
    def emptyObj[K]():Object[K] = Object(Nil)



enum All[Key]:
  case Empty[K]() extends All[K]
  case Object(props : L[Prop[Key]] )
  case Value[K,T](v :T) extends All[K]
  case List[K,T](value : L[T]) extends All[K]

  inline def /(s : Key) = apply(s)

  def replace(newK : Key, old : Key):All[Key] = 
    this match 
      case o : Object[Key] => 
        o.copy(o.props.map{
          case e if e.key == old => e.copy(key = newK)
          case o => o
        })
      case _ => this
  def setEmpty(p  : Key):All[Key] = set(p,Empty())
  def setEmpty(p  : Path[Key]):All[Key] = 
    set(p,Empty())
  def set(p  : Path[Key], elmt : All[Key]) :All[Key] = 
      if p.values.size == 1 then set(p.values.head,elmt)
      else
        this match {
        case o : Object[Key] => 
          o.props.find(_.key == p.values.head).map{
            prop => 
              val updated = prop.value.set(Path(p.values.tail),elmt)
              o.copy(o.props.filter(_.key != p.values.head):+ All.ObjectProp(p.values.head, updated))
          }.getOrElse(o)
        case _ => this
    }
  def set(p  : Key, elmt : All[Key]) :All[Key] = 
     this match {
      case o : Object[Key] => o.copy(o.props.filter(_.key != p) :+ ObjectProp(p,elmt))
      case _ => this
    }

  def apply(s : Key) : All[Key] =
    this match {
      case Object(props) => props.find(_.key == s).get.value
      case _ => Empty[Key]()
    }

  def apply(s : Path[Key]) : All[Key] =
    if s.values.size == 1 then /(s.values.head)
    else if s.values.isEmpty then All.Empty()
    else apply(s.values.head).apply(Path(s.values.tail))





