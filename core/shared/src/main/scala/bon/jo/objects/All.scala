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
import scala.collection.immutable.List
import bon.jo.utils.Tree
import bon.jo.words.Phrase.toTree
import bon.jo.utils.Tree.EndTree
import scala.annotation.tailrec
import bon.jo.words.Phrase.ParsingException
import bon.jo.objects.StringExctractor.CharPos

/**
 * Object model.
 * @see [[AllOps]] for methods
*/
enum All[Key] extends AllOps[Key]:
  case Empty[K]() extends All[K] with NoObjectOps[K]
  case ObjectAll(props : List[Prop[Key]] )  extends All[Key] with AllObjectOps[Key]
  case Value[K,+T](_value :T) extends All[K] with NoObjectOps[K]
  case ListAll[K](value : List[All[K]]) extends All[K]  with NoObjectOps[K]
/** Factory for [[All]] instances. 
 * Can create [[All]] from json string with  `All.apply(String)`
*/
object All:
  type Prop[Key] = ObjectProp[Key,All[Key]]
  /**
   * Specialisation of [[All]]`[`String`]`
  */
  extension (e : All[String])
    
    def toJsonString():String = jsonString(e)
  /**
  * Shortand to [[All]]`[`String`]`, like json
  */
  type AllString = All[String]


  type BuffFlow = Appendable ?=> Appendable
  private def buff : BuffFlow = summon


  private inline def invalid(message : String,pos : PosPe) = throw new ParsingException(message,pos)

  private enum ParseState{case InObject,InArray,Root,InProp}

  private case class ObjectBuilder(obj : ObjectAll[String] = Dsl.emptyObj()) extends Builder[ ObjectAll[String]]:
    def add(prp : ObjectProp[String,All[String]]) : ObjectBuilder = ObjectBuilder(obj.copy(obj.props :+ prp))
  private case class ArrayBuilder(obj : ListAll[String] = ListAll(Nil)) extends Builder[ ListAll[String]] :
    def add(prp : All[String]) : ArrayBuilder = ArrayBuilder(obj.copy(obj.value :+ prp))  
  private case class PropBuilder(obj : ObjectProp[String,All[String]] = ObjectProp("",Dsl.emptyObj())) extends Builder[ ObjectProp[String,All[String]]]:
    def add(prp : All[String]) :PropBuilder = copy(obj.copy(value = prp))
  private object NoneBuilder extends Builder[Nothing]:
    def obj:Nothing = ???
    def add(prp: All[String]): Builder[Nothing] = ???
  private sealed trait Builder[+T]:
    def obj : T
  private case class Ctx(
    builder : Builder[All[String] | ObjectProp[String,All[String]]] = NoneBuilder,
    parent : Option[Ctx] = None,
    state : ParseState = ParseState.Root,
    deep : List[PosPe] = Nil
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
        val nObj : ObjectAll[String] = parentBuilder.obj.copy(parentBuilder.obj.props :+ prop)
        parent.get.copy(parentBuilder.copy(nObj))
      inline def removeLastDeep(ctx : Ctx) = ctx.copy(deep = deep.dropRight(1)) 
      def arrayValue(v : All[String]):Ctx = 
        copy(arrayBuilder.copy( arrayBuilder.obj.copy(arrayBuilder.obj.value :+ v) ))

      def closeArray() : Ctx = 
        removeLastDeep{
          (state,builder) match
            case (ParseState.InArray,ArrayBuilder(array) )=>
              val parentVal = parent.get
              parentVal.state match
                case ParseState.InArray => parentVal.arrayValue(array)
                case ParseState.InProp => parentVal.propValue(array)
        }

      def closeObject() : Ctx = 
        removeLastDeep{
          (state,builder) match
            case (ParseState.InObject,ObjectBuilder(obj) )=>
              val parentVal = parent.get
              parentVal.state match
                case ParseState.InArray => parentVal.arrayValue(obj)
                case ParseState.InProp => parentVal.propValue(obj)
                case ParseState.Root => this
        }

  /**
  * Returns [All[String]] from json String.
  */
  def apply(s : String):All[String] = 
    val res2 : List[PosPe] =  StringExctractor.parse(s).stringEscape().removeSpace()
    val ctx = res2.foldLeft(Ctx()){ (ctx,pe) => 
      pe.value match
        case `{` => Ctx(ObjectBuilder(),Some(ctx),ParseState.InObject,ctx.deep :+ pe)
        case  PhraseElement.Symbol("[") => Ctx(ArrayBuilder(),Some(ctx),ParseState.InArray,ctx.deep :+ pe)
        case PhraseElement.Symbol("]") => 
          ctx.closeArray()
        case `}` => 
          ctx.closeObject()   
        case PhraseElement.Text(w) =>
          ctx.state match
            case ParseState.InProp => ctx.propValue(All.Value(w)) 
            case ParseState.InArray => ctx.arrayValue(All.Value(w)) 
            case ParseState.InObject => 
               Ctx(PropBuilder(ObjectProp(w,Dsl.emptyObj())),Some(ctx),ParseState.InProp)
        case PhraseElement.Word(w) =>
          ctx.state match
            case ParseState.InObject => 
               Ctx(PropBuilder(ObjectProp(w,Dsl.emptyObj())),Some(ctx),ParseState.InProp)
            case ParseState.InProp if w == "null" => ctx.propValue(All.Empty()) 
            case ParseState.InProp => invalid(s"invalid token : $w",pe)
            case ParseState.InArray => ctx.arrayValue(All.Value(w))     
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
    if ctx.deep.nonEmpty then throw new ParsingException("open but not close",ctx.deep.last)
    ctx.builder.obj.asInstanceOf[All[String]]

  /**
   * Returns json string represntation of `p`
  */
  def jsonString(p : AllString): String =
    given Appendable = StringBuilder().underlying
    toJson(p).toString
  def toJson(p : AllString): BuffFlow = 
    p match
      case All.ObjectAll(prop) => 
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
              
          case o : (Int | Float | Boolean | Long | Double | Short)  => buff.append(o.toString)
          case null => buff.append("null")
          case s   => buff.append(s""""${s.toString.replace("\"","\\\"")}"""")     
      case All.ListAll(v) =>
        buff.append("[")
        v.zipWithIndex.foreach{(pr,i) =>
          toJson(pr)
          if(i != v.size -1 ) then
            buff.append(",")
        }
        buff.append("]")
      case All.Empty() => buff.append("null")
    buff
  case class ObjectProp[K,+V <: All[K]](key : K, value: V)



  case class Path[K](values : List[K]):
    def /(s : K): Path[K] = Path(values :+ s)
    def paths : Seq[Path[K]] = for (i <- 0 until values.length ) yield Path(values.slice(0,i+1))
  object Path:
    extension [K](pathStart : K)
      def /(pathNext : K) : Path[K] = Path(List(pathStart,pathNext))
  object Dsl:

    extension [K](s : K)
      def :=(v : All[K]):  OnBuild[K] =
        buff.value =  buff.value.copy(buff.value.props.filter(_.key != s) :+ ObjectProp(s,v))
        buff

      def :=[V](v : V):  OnBuild[K] =
        s := All.Value[K,V](v)
        buff

    case class Buff[A](var value : All.ObjectAll[A])
    type Flow[K,A] = Buff[K] ?=> A
    type OnBuild[K] = Flow[K,Buff[K]]
    def buff[K] : OnBuild[K] = summon
    def buildValue[K] : Flow[K,All.ObjectAll[K]] = buff.value

    def list[K](objs : All[K] *) = All.ListAll(objs.toList)
    def obj[K](builds : OnBuild[K] * ):All.ObjectAll[K] =
      given  Buff[K] = Buff[K](All.ObjectAll(Nil))
      builds.foreach(build => build)
      buildValue
    def emptyObj[K]():ObjectAll[K] = ObjectAll(Nil)




 







