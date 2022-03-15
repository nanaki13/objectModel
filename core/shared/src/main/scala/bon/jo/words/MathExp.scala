package bon.jo.words

import scala.annotation.tailrec
import bon.jo.utils.Tree
import bon.jo.utils.Tree.EndTree
import bon.jo.words.{MathExp => M}
import PhraseElement.given
import bon.jo.words.Phrase.*
import bon.jo.words.PhraseElement.*
import bon.jo.words.MathExp.given
import bon.jo.objects.StringExctractor
object MathExp:

  def apply(s :String):Tree[M] = 
 
    val res2 : List[PosPe] =  StringExctractor.parse(s).removeSpace().numberFormat()

    val tree : Tree.Node[PosPe] = res2.toTree(`(`,`)`)
    //println(tree)
    tree.map[M](M.apply)
 
  extension (e : Tree[M])

    def resolveParamFunction():Tree[M] =  
      e.mapNode{
        case o @  Tree.Node(v) => 
          o.copy(replaceFunc(o.childs) )

        case a => a
      }
    def compile( ) :  Tree[M]   = 
      calculate_(resolveParamFunction())
    def single:Option[M] = 
      e match 
        case EndTree(List(m)) => Some(m)
        case _ => None
    def evaluate():Double = tryApply(Map.empty)
    def tryApplyMap(param : Map[String,Double]):Double = 
      tryApply(param.map(PhraseElement.Word(_)-> Number(_,null)))
    def tryApply(param : Map[PhraseElement,Number]):Double = 
      e match 
        case  EndTree(Nil) => Double.NaN 
        case EndTree(List( v)) => v.evaluate(param).value
        case EndTree(c) => if c.headOption.exists(_.isOperator) then throw new ParsingException(s"illegal start : ",c.head.source)
         else if c.lastOption.exists(_.isOperator) then throw new ParsingException(s"illegal end : ",c.last.source) else throw  new ParsingException(s"can't evaluate $e")
        case o  => throw  new ParsingException(s"can't evaluate $e")
        
    def replaceFunc(vv : List[ Tree[MathExp]] ): List[ Tree[MathExp]] = 
      val endIndex = vv.indexWhere{
        case EndTree(vv) => vv.lastOption.map{
          case  vf : MathExp.Function if vf.param == Tree.root => 
            true
          case _ => false
        
      }.getOrElse(false)
        case o => false
      }
      if endIndex != -1 then
        var removFunc  = vv(endIndex).asInstanceOf[EndTree[MathExp]]
        var next  = vv(endIndex+1).asInstanceOf[Tree.Node[MathExp]].mapNode{
          case o @  Tree.Node(v) => 
            o.copy(replaceFunc(o.childs) )
          case a => a
        }.asInstanceOf[Tree.Node[MathExp]]
        val function = removFunc.values.last.asInstanceOf[MathExp.Function]
        next = next.copy(List(Tree.EndTree(List(function.copy(param = next)))))
        removFunc  = removFunc.copy(removFunc.values.dropRight(1))
        val res = replaceFunc(vv.slice(0,endIndex) ++ List(removFunc,next)++vv.slice(endIndex+2,vv.size))
        res
      else 
        vv
        

  private def calculate_(mathTree :Tree[M] ): Tree[M]=
    val red  = mathTree.applyToChilds{e =>
      e.zipWithIndex.foreach{
          (ch,i) => 
            val next = if i < e.size -1 then Some(e(i+1)) else None
            (ch,next) match
              case (s: Symbol,Some(v )) if !v.isOperator =>throw new ParsingException("two symbol or number without operation",s.source,v.source) 
              case (s: Number,Some(v ))  if !v.isOperator =>throw new ParsingException("two symbol or number without operation : ",s.source,v.source) 
              case (s: Op,Some(v:Op ))   =>throw new ParsingException(s"two operators : ",s.source,v.source) 
              case _ =>
        }
      if e.exists(_.isFunction)  then 
        e.map{
         
          case f : MathExp.Function => 
            val params = calculate_(f.param)
            val symb = params match 
              case EndTree(List(FunctionMathExp(s,_,_))) => s
              case EndTree(l) => 
                l.filter(_.isSymbol).map(_.source.value).toSet
              case o => Set.empty
            f.copy(param = params).toFunctionMathExp.copy(symbols = symb)
          case o => o
        }
      else if e.headOption.exists(!_.isOperator) && e.lastOption.exists(!_.isOperator) then 
        resolveFirstPrio(e)
      else
        e
    }
    
    val red2 = red.mapNode{
      case Tree.Node(List(Tree.EndTree(List(M.Number(r,s))))) => Tree.EndTree(List(M.Number(r,s)))
      case Tree.Node(List(Tree.EndTree(List(M.FunctionMathExp(r,p,s))))) => Tree.EndTree(List(M.FunctionMathExp(r,p,s)))
      case o => o
    }
    
    val red3= red2.mapNode{ n =>
      if n.childs.count(_.isInstanceOf[EndTree[_]]) == n.childs.size then 
        var ch = n.childs.flatMap{
          case e : EndTree[M] => e.values
          case _ => ???
        }
        ch = if ch.headOption.exists(_.isUnaryOp) && ch.size > 1 then
          ch.head.asUnaryOperator.apply(ch(1)) :: ch.drop(2)
        else ch
        
        Tree.EndTree(ch) 
      else n  
    }
   
    if(red3 == mathTree) then mathTree
    else calculate_(red3)
  def resolve(idxOp : Int,v : List[M]):List[M] = 
    if idxOp != -1 then
      val op = v(idxOp).asInstanceOf[M.Op]
      val l = v(idxOp-1)
      val r = v(idxOp+1)
    
      val calc = op(l,r)
      ( v.slice(0,idxOp-1):+calc )++ v.slice(idxOp+2,v.size) 
    else 
      v

  @tailrec
  def resolveFirstPrio(v : List[M]):List[M] = 
    v match 
      case List(M.Number(n,_)) => v
      case _  => 
        val idxM = v.indexWhere( _ == M.* )
        val idxD = v.indexWhere(_ == M./ )
        val otherFirst = v.indexWhere(e => e.isOperator)
        
        val pIdx = if idxM != -1 then idxM else if idxD != -1 then idxD else otherFirst 
        val res = resolve(pIdx,v)
        if res == v then v
        else resolveFirstPrio(resolve(pIdx,v))



  val stringFunction : Map[String,Double=>Double] = Map("cos" -> Math.cos,"sin"->Math.sin,"tab"->Math.tan,"ln"->Math.log)
  def apply(p : PosPe):M = 
    p match 
      case v@PositionedPhraseElement(pos, PhraseElement.+) => new +(v)
      case v@PositionedPhraseElement(pos, PhraseElement.-) => new -(v)
      case v@PositionedPhraseElement(pos, PhraseElement./) => new /(v)
      case v@PositionedPhraseElement(pos, PhraseElement.*) =>  new *(v)
      case a@PositionedPhraseElement(pos,PhraseElement.Number(v)) =>  Number(v.toDouble,a)
      case w@PositionedPhraseElement(pos,PhraseElement.Word(v))  if !stringFunction.contains(v) => Symbol(v,w)
      case  w@PositionedPhraseElement(pos,PhraseElement.Word(v)) => Function(v,Tree.root,w)
      case _ => throw new ParsingException("invalid math exp : ",p)
  case class Function(value : String,param:Tree[MathExp],source : PosPe) extends MathExp :
    def evaluate( p :Map[PhraseElement,MathExp.Number]) = 
      MathExp.Number( stringFunction(value)(param.tryApply(p)),null)
    def toFunctionMathExp : FunctionMathExp = 
      FunctionMathExp(fun = p =>evaluate(p),source)
    
    def isNumber : Boolean = false
    def isOperator : Boolean = false
    def isFunction : Boolean = true
  case class Symbol(value : String,source :PosPe) extends MathExp :
    def isNumber : Boolean = false
    def isOperator : Boolean = false
    def evaluate(p : Map[PhraseElement,Number])= p(this.source.value)
    def isFunction : Boolean = false
    override def isSymbol: Boolean = true

  //case class FunctionMathExp(symbol : Symbol,fun : Number => Number) extends MathExp
  case class FunctionMathExp(symbols : Set[PhraseElement] = Set.empty,fun : Map[PhraseElement,Number] => Number,source : PosPe) extends MathExp:
    def isNumber : Boolean = false
    def isOperator : Boolean = false
     def isFunction : Boolean = false
    def evaluate(p : Map[PhraseElement,Number])= fun(p)
  trait UnaryOp extends Op:
    def neutral:Number
    def apply(l : M):M = 
      this(neutral,l)
  trait Op extends M:
    val symbol : String
    override def toString():String = s"Op($symbol)"
    def evaluate(p : Map[PhraseElement,Number]) : Number= throw new ParsingException("can't evaluate single oprerator : ",source)
    def isNuary : Boolean = false
    def apply(l : Number): Number => Number
    extension (mm : Symbol)
      def sv = mm.source.value
    def apply(left : M,right :  M): M=

      (left,right) match
        case (a:Number,b:Number) => apply(a)(b)
        case (a:Symbol,b:Number) => FunctionMathExp(Set(a.sv),l => apply(l(a.sv))(b),a.source)
        case (a:Symbol,b:Symbol) => FunctionMathExp(Set(a.sv,b.sv),l => apply(l(a.sv))(l(b.sv)),a.source)
        case (a:Number,b:Symbol) => FunctionMathExp(Set(b.sv),l =>apply(a)(l(b.sv)),a.source)
        case (a:Number,b:FunctionMathExp) => FunctionMathExp(b.symbols,l =>apply(a)(b.fun(l)),a.source)
        case (a:Symbol,b:FunctionMathExp) => FunctionMathExp(b.symbols + a.sv,l =>apply(l(a.sv))(b.fun(l)),a.source)
        case (a:FunctionMathExp,b:Number) => FunctionMathExp(a.symbols,l =>apply(a.fun(l))(b),a.source)
        case (a:FunctionMathExp,b:Symbol) => FunctionMathExp(a.symbols + b.sv,l =>apply(a.fun(l))(l(b.sv)),a.source)
        case (a:FunctionMathExp,b:FunctionMathExp) => FunctionMathExp(a.symbols++b.symbols ,l =>apply(a.fun(l))(b.fun(l)),a.source)
    def isNumber : Boolean = false
    def isOperator : Boolean = true
    def isFunction : Boolean = false
  val _0 :  M.Number = M.Number(0,null)
  case class+(source : PosPe)  extends M with UnaryOp:
    val symbol : String = PhraseElement.+.value
    override def isNuary : Boolean = true
    val neutral = _0
    def apply(l : Number): Number => Number = r => Number(l.value + r.value,r.source)
  case class/(source : PosPe) extends M with Op:
    val symbol : String = PhraseElement./.value
    def apply(l : Number): Number => Number = r => Number(l.value / r.value,r.source)
  case class-(source : PosPe) extends M with UnaryOp:
    val symbol : String = PhraseElement.-.value

    val neutral = _0
    override def isNuary : Boolean = true
    def apply(l : Number): Number => Number = r => Number(l.value - r.value,r.source)
  case class *(source : PosPe) extends M with Op:
    val symbol : String = PhraseElement.*.value
    def apply(l : Number): Number => Number = r => Number(l.value * r.value,r.source)
  case class Number(value : Double,source :PosPe ) extends M:
    def this(v : Double) = this(v,null)
    def isNumber : Boolean = true
    def isOperator : Boolean = false
    def isFunction : Boolean = false
    def apply(op : Op,r : Number) = op(this)(r)
    def evaluate( p :Map[PhraseElement,MathExp.Number]) = this
  object NumberValue:
    def unapply(n : MathExp):Option[Double] = 
      n match 
        case Number(nn,_) => Some(nn)
        case _ => None

end MathExp
sealed trait MathExp:
  def isNumber : Boolean
  def isOperator : Boolean
  def isFunction : Boolean
  def isSymbol : Boolean = false
  def source : PosPe
  def constantValue :MathExp.Number = evaluate(Map.empty)
  def evaluate( p :Map[PhraseElement,MathExp.Number]):MathExp.Number
  inline def isUnaryOp:Boolean = if isOperator then asOperator.isNuary else false
  def asOperator : MathExp.Op = this.asInstanceOf[MathExp.Op]  
  def asUnaryOperator : MathExp.UnaryOp = this.asInstanceOf[MathExp.UnaryOp]  
end MathExp