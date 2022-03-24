package bon.jo.objects
import All.*
import bon.jo.words.PhraseElement
import bon.jo.words.Phrase.removeSpace
import bon.jo.words.Phrase.numberFormat
import bon.jo.words.Phrase.stringEscape
import bon.jo.words.PhraseElement.PosPe
import bon.jo.words.PhraseElement.given
import bon.jo.words.PhraseElement.`{`
import bon.jo.words.PhraseElement.`}`
import bon.jo.words.PhraseElement.`"`
import java.time.LocalDate
import scala.collection.immutable
import bon.jo.utils.Tree
import bon.jo.words.Phrase.toTree
import bon.jo.utils.Tree.EndTree
import scala.annotation.tailrec

object All:

  type AllString = All[String]
  type LList[A] = scala.collection.immutable.List[A]
  val LList = scala.collection.immutable.List
  type BuffS = StringBuilder ?=> StringBuilder
  def buff : BuffS = summon

  def nextVal(l : LList[PosPe]):(All[String], LList[PosPe]) = 
    if l.isEmpty then (Dsl.emptyObj(),Nil)
    else

      def value(start : PhraseElement, tail : LList[PosPe]):(All.Value[String,Any], LList[PosPe]) = 
      
        start match
              case PhraseElement.Word(n) => (All.Value(n),tail)
              case PhraseElement.Number(n) => (All.Value(n.toDouble),tail)
      l match 
        case a :: tail => 
          value(a.value,tail)


  inline def invalid(r : Any) = throw IllegalStateException(s"invalid : $r")
  def nextKey(l : LList[PosPe]):(String, LList[PosPe]) = 

    l match 
        case a :: tail if a.value == PhraseElement.Symbol(",") => nextKey(l.tail)
        case b :: d :: tail => 
              b.value match
                case PhraseElement.Word(w) => 

                  (w,tail)
  def nextProp(l : LList[PosPe]):(ObjectProp[String,All[String]], LList[PosPe]) = 

    val (key,afterKey) = nextKey(l)

    val  (value,afterValue) = nextVal(afterKey)
    (ObjectProp(key,value),afterValue)

  @tailrec
  def reduceToObject(done : LList[ObjectProp[String,All[String]]] = Nil)(l : LList[PosPe]) : LList[ObjectProp[String,All[String]]] = 
    if(l.isEmpty) then
      done
    else
      val (prp,todo) = nextProp(l)
      reduceToObject(done :+ prp)(todo)
  
  case class Ctx(build : EndTree[ObjectProp[String, All[String]]])
  def attachPropToObject(e :  LList[Tree[ObjectProp[String, All[String]]]]):  LList[Tree[ObjectProp[String, All[String]]]] =

    if e.size == 1 then 
      e.head match
        case Tree.Node(childs) => attachPropToObject(childs)
        case o => e
    else
      e.foldLeft(Ctx(EndTree(Nil))){
        (ctx,el) =>
          el match
            case n : Tree.Node[ObjectProp[String, All[String]]] if ctx.build.values.nonEmpty =>  
              var obj = ctx.build.values.last.asInstanceOf[ObjectProp[String,Object[String]]]
              
              obj = obj.copy(value = obj.value.copy(attachPropToObject(n.childs).head.asInstanceOf[EndTree[ObjectProp[String, All[String]]]].values))
              ctx.copy(ctx.build.copy( ctx.build.values.dropRight(1):+obj))
            case n :  Tree.EndTree[ObjectProp[String, All[String]]] => ctx.copy(ctx.build.copy(ctx.build.values ++ n.values))
            case o => println(o);???
      }.build :: Nil

  def apply(s : String):Object[String] = 
    val res2 : scala.collection.immutable.List[PosPe] =  StringExctractor.parse(s).stringEscape().removeSpace().numberFormat()

    val tree : Tree.Node[PosPe] = res2.toTree(`{`,`}`)

    val partialObjTree = tree.applyToChilds{
     reduceToObject(Nil)
    }

    val attached = attachPropToObject(partialObjTree.childs)

    attached match
      case LList(EndTree(l)) => Object(l)
      case o => println( o);???

  def toJsonString(p : AllString): String =
    given StringBuilder = StringBuilder()
    toJson(p).toString
  def toJson(p : AllString): BuffS = 
    p match
      case All.Object(prop) => 
        buff.append("{")
        prop.zipWithIndex.foreach{(pr,i) =>
          buff.append(s""""${pr.key}":""")      
          toJson(pr.value)
          if(i != prop.size -1 ) then
            buff.append(",")
        }
        buff.append("}")
      case All.Value(v) =>
        v match
          case s : (String | LocalDate )=> buff.append(s""""${s.toString.replace("\"","\\\"")}"""")         
          case o  => buff.append(o)
      case All.List(v) =>
        buff.append("{")
        v.zipWithIndex.foreach{(pr,i) =>
          toJson(pr)
          if(i != v.size -1 ) then
            buff.append(",")
        }
        buff.append("]")
      case All.Empty() => buff.append("null")
      //case All.
    buff
  case class ObjectProp[K,+V <: All[K]](key : K, value: V)
  type L[A] = scala.collection.immutable.List[A]
  val L: immutable.List.type = scala.collection.immutable.List
  type Prop[Key] = ObjectProp[Key,All[Key]]

  @main
  def toto() =
    type Obj = All[String]
    import Dsl.*
    val objee : Obj= obj{
      "name test" := "bob"
      "groupe" := obj{
        "name" := "best"
        "familly" := obj{
           "id" := 1
        }
      }
      "birthDate" := LocalDate.of(2000,1,1)
    }
    println(objee / "groupe"  / "name")
    println(toJsonString(objee))
    val objF = All(toJsonString(objee))
    println(objF)
    println(toJsonString(objF))


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
  case List[K](value : L[All[K]]) extends All[K]

  inline def /(s : Key) = apply(s)

  def contains(e : Key): Boolean = 
    this match
      case o : Object[Key] => o.props.exists(_.key == e)
      case _ => false 

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
      if p.values.isEmpty then elmt
      else if p.values.size == 1 then set(p.values.head,elmt)
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
    if s.values.isEmpty then this
    else if s.values.size == 1 then /(s.values.head)
    else if s.values.isEmpty then All.Empty()
    else apply(s.values.head).apply(Path(s.values.tail))







