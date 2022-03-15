package bon.jo.words
import bon.jo.objects.StringExctractor.{Builder, CharPos}
 import bon.jo.objects.StringExctractor.Compat
 import bon.jo.utils.InputsToOutput
 import bon.jo.utils.ConvertList
 
sealed trait PhraseElement:
  val value : String
  def copy(value : String):PhraseElement
    

object PhraseElement:
  type PosPe = PositionedPhraseElement[ PhraseElement]
  case class PositionedPhraseElement[+T<:PhraseElement](pos : Int,value : T)
  object StringEscape:
    val quote =  PhraseElement.Symbol("\"")
    

    given InputsToOutput[PosPe,PosPe] = InputsToOutput(bf = buff  =>  PositionedPhraseElement[PhraseElement.Word](buff.head.pos,
      PhraseElement.Word(buff.map(_.value.value).mkString)),acceptF = (buff,el ) => {
        el.value match
          case PhraseElement.Symbol("\"") if !buff.map(_.value).contains(quote) => true
          case p : PhraseElement if !buff.map(_.value).contains(quote) => false   
          case PhraseElement.Symbol("\"") if buff.map(_.value).lastOption != Some( PhraseElement.Symbol("\\")) => true
          case _ => !( buff.map(_.value).headOption == Some(quote) && buff.map(_.value).lastOption == Some(quote) && buff.size > 1 )   
          
    },refuse = ((e : PosPe) => e) )

    def apply(l : List[PosPe]):List[PosPe] = ConvertList(l)
  object NumberReduce:
    def haveNumber(b : List[PosPe]): Boolean = b.map(_.value).exists{
      case e :  PhraseElement.Number => true
      case _ => false
    }
    def noSymbol(b : List[PosPe]): Boolean = b.map(_.value).count{
      case e :  PhraseElement.Symbol => true
      case _ => false
    } == 0
    given InputsToOutput[PosPe,PosPe] = InputsToOutput(bf = buff  => PositionedPhraseElement[PhraseElement.Number](buff.head.pos,PhraseElement.Number(buff.map(_.value.value).mkString)),acceptF = (buff,el ) => {
      el.value match
       // case PhraseElement.- if buff.isEmpty  => true
        case e : PhraseElement.Number => true
        case PhraseElement.Symbol(",") if haveNumber(buff) && noSymbol(buff) => true
        case PhraseElement.Symbol(".") if haveNumber(buff) && noSymbol(buff) => true
        case _ => false
    },refuse = ((e : PosPe) => e) )
      
    def apply(l : List[PosPe]):List[PosPe] = ConvertList(l)
  end NumberReduce 
  given Compat[PositionedPhraseElement[PhraseElement]] with
    
    def add(t:  Builder[PositionedPhraseElement[PhraseElement]], c: CharPos): Builder[PositionedPhraseElement[PhraseElement]] = 
      t.value.append(c.current)
      t
    def create(c: CharPos):Builder[PositionedPhraseElement[PhraseElement]] = 
      val ch = c.current
      if ch.isSpaceChar then  PhraseElementBuilder.SpaceBuilder(c.pos,StringBuilder(c.current.toString))
      else if c.current.isLetter then  PhraseElementBuilder.WordBuilder(c.pos,StringBuilder(c.current.toString))
      else if c.current.isDigit  then  PhraseElementBuilder.NumberBuilder(c.pos,StringBuilder(c.current.toString))
      else PhraseElementBuilder.SymbolBuilder(c.pos,StringBuilder(c.current.toString)) 
  val `(` = PhraseElement.Symbol("(") 
  val `)` = PhraseElement.Symbol(")") 
  val + = PhraseElement.Symbol("+") 
  val - = PhraseElement.Symbol("-")
  val * = PhraseElement.Symbol("*")
  val / = PhraseElement.Symbol("/")
  object Empty  extends PhraseElement:
    val value = ""
    def copy(value : String) : PhraseElement = Empty

  case class Word(value : String) extends PhraseElement
  case class Number(value : String) extends PhraseElement
  case class Space(value : String) extends PhraseElement
  case class Symbol(value : String) extends PhraseElement
  case class Unknown(value : String) extends PhraseElement
  object PhraseElementBuilder:

    case class WordBuilder(pos : Int,value  :StringBuilder ) extends Builder[PositionedPhraseElement[PhraseElement.Word]]:
      def compatible(c:CharPos): Boolean = c.current.isLetter
      def build:PositionedPhraseElement[PhraseElement.Word] = PositionedPhraseElement(pos,PhraseElement.Word(value.toString))
    case class NumberBuilder(pos : Int,value  :StringBuilder ) extends Builder[PositionedPhraseElement[PhraseElement.Number]]:
      def compatible(c:CharPos): Boolean = c.current.isDigit
      def build:PositionedPhraseElement[PhraseElement.Number] = PositionedPhraseElement(pos,PhraseElement.Number(value.toString))
    case class SpaceBuilder(pos : Int,value  :StringBuilder ) extends Builder[PositionedPhraseElement[PhraseElement.Space]]:
      def compatible(c:CharPos): Boolean = c.current.isSpaceChar
      def build:PositionedPhraseElement[PhraseElement.Space] = PositionedPhraseElement(pos,PhraseElement.Space(value.toString))
    case class SymbolBuilder(pos : Int,value  :StringBuilder ) extends Builder[PositionedPhraseElement[PhraseElement.Symbol]]:
      def compatible(c:CharPos): Boolean = !c.current.isSpaceChar && !c.current.isLetter && !c.current.isDigit && value.isEmpty
      def build:PositionedPhraseElement[PhraseElement.Symbol] = PositionedPhraseElement(pos,PhraseElement.Symbol(value.toString))
    case class UnknownBuilder(pos : Int,value  :StringBuilder ) extends Builder[PositionedPhraseElement[PhraseElement.Unknown]]:
      def build:PositionedPhraseElement[PhraseElement.Unknown] = PositionedPhraseElement(pos,PhraseElement.Unknown(value.toString))
      def compatible(c:CharPos): Boolean = false