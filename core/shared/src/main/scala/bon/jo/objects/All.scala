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


  inline def invalid(r : Any) = throw IllegalStateException(s"invalid : $r")

  enum ParseState{case InObject,InArray,Root,InProp}

  case class ObjectBuilder(obj : Object[String] = Dsl.emptyObj()) extends Builder[ Object[String]]:
    def add(prp : ObjectProp[String,All[String]]) : ObjectBuilder = ObjectBuilder(obj.copy(obj.props :+ prp))
  case class ArrayBuilder(obj : List[String] = List(Nil)) extends Builder[ List[String]] :
    def add(prp : All[String]) : ArrayBuilder = ArrayBuilder(obj.copy(obj.value :+ prp))  
  case class PropBuilder(obj : ObjectProp[String,All[String]] = ObjectProp("",Dsl.emptyObj())) extends Builder[ ObjectProp[String,All[String]]]:
    def add(prp : All[String]) :PropBuilder = copy(obj.copy(value = prp))
  object NoneBuilder extends Builder[Nothing]:
    def obj:Nothing = ???
    def add(prp: All[String]): Builder[Nothing] = ???
  sealed trait Builder[+T]:
    def obj : T
  case class Ctx(
    builder : Builder[All[String] | ObjectProp[String,All[String]]] = NoneBuilder,
    parent : Option[Ctx] = None,
    state : ParseState = ParseState.Root
    ):
      def propBuilder: PropBuilder = 
        builder.asInstanceOf[PropBuilder]
      def objectBuilder: ObjectBuilder = 
        builder.asInstanceOf[ObjectBuilder]
      def arrayBuilder: ArrayBuilder = 
        builder.asInstanceOf[ArrayBuilder]

      def propValue(v : All[String]):Ctx = 
        val prop = propBuilder.obj.copy(value = v)
        val parentBuilder = parent.get.objectBuilder
        val nObj : Object[String] = parentBuilder.obj.copy(parentBuilder.obj.props :+ prop)
        parent.get.copy(parentBuilder.copy(nObj))
      def arrayValue(v : All[String]):Ctx = 
        copy(arrayBuilder.copy( arrayBuilder.obj.copy(arrayBuilder.obj.value :+ v) ))

      def closeArray() : Ctx = 
        (state,builder) match
          case (ParseState.InArray,ArrayBuilder(array) )=>
            val parentVal = parent.get
            parentVal.state match
              case ParseState.InArray => parentVal.arrayValue(array) 
              case ParseState.InProp => parentVal.propValue(array) 
      def closeObject() : Ctx = 
        (state,builder) match
          case (ParseState.InObject,ObjectBuilder(obj) )=>
            val parentVal = parent.get
            parentVal.state match
              case ParseState.InArray => parentVal.arrayValue(obj) 
              case ParseState.InProp => parentVal.propValue(obj) 
              case ParseState.Root => this
     
    
  def apply(s : String):All[String] = 
    val res2 : scala.collection.immutable.List[PosPe] =  StringExctractor.parse(s).stringEscape().removeSpace()
    val ctx = res2.foldLeft(Ctx()){ (ctx,pe) => 
      pe.value match
        case `{` => Ctx(ObjectBuilder(),Some(ctx),ParseState.InObject)
        case  PhraseElement.Symbol("[") => Ctx(ArrayBuilder(),Some(ctx),ParseState.InArray)
        case PhraseElement.Symbol("]") => 
          ctx.closeArray()
        case `}` => 
          ctx.closeObject()                                   
        case PhraseElement.Word(w) =>
          ctx.state match
            case ParseState.InObject => 
               Ctx(PropBuilder(ObjectProp(w,Dsl.emptyObj())),Some(ctx),ParseState.InProp)
            case ParseState.InProp => ctx.propValue(All.Value(w)) 
            case ParseState.InArray => 
              ctx.arrayValue(All.Value(w)) 
        case PhraseElement.Number(w) =>
          val f = w.toDouble
          val o = if f.isFinite then f.toLong else f 
          ctx.state match
            case ParseState.InObject => 
               Ctx(PropBuilder(),Some(ctx),ParseState.InProp)
            case ParseState.InProp => 
             
              ctx.propValue(All.Value( o)) 
            case ParseState.InArray => 
              ctx.arrayValue(All.Value(o)) 
        case PhraseElement.Symbol(":") => 
          ctx.state match
            case ParseState.InProp => ctx     
        case PhraseElement.Symbol(",") => 
          ctx.state match
            case ParseState.InObject |ParseState.InArray   => ctx
            //  ret  
              
         
           

         
    }
    ctx.builder.obj.asInstanceOf[All[String]]

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
        buff.append("[")
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
        "listA" := list(All.Value[String,Integer](1),obj{
           "id" := 1
        })
      }
      "birthDate" := LocalDate.of(2000,1,1)
    }

    println(toJsonString(objee))
    val objF = All(toJsonString(objee))
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

    def list[K](objs : All[K] *) = All.List(objs.toList)
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







