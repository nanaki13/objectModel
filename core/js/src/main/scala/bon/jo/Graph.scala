package bon.jo

import bon.jo.words.MathExp
import bon.jo.words.PhraseElement

object Graph:
  enum Dir:
    case middle,left,right,top,bottom
  extension (v  :Double)
    def scaleToMe(min : Double,max:Double,translation : Double)(value : Double):Double = (value+translation)*v/(max -min ) 
  case class GraphParam(s : PhraseElement,min : Double,max : Double,step : Double,axePostionX : Dir,axePostionY :Dir):
    def this(s : PhraseElement,min : Double,max : Double,nbPoint : Int,axePostionX : Dir ,axePostionY :Dir ) = this(s,min,max,(max-min)/nbPoint.toDouble,axePostionX,axePostionY)
  def values(f : MathExp.FunctionMathExp,p : GraphParam):List[EvaluedFun1] = 
    val fParam = Map(p.s->new MathExp.Number(0))
    
    Iterator.iterate(p .min)(_ + p.step).takeWhile(_ <= p.max)
    .map(v => EvaluedFun1.apply(v ,f.evaluate(Map(p.s->new MathExp.Number(v))).value))
    .toList

  opaque type EvaluedFun1 = (Double,Double)
  object EvaluedFun1:
    def apply(p : Double,fv: Double):EvaluedFun1 = p -> fv
    extension (v  :EvaluedFun1)
      def paramVal =v._1
      def funVal =v._2
      def copy(p : Double,fv: Double) = v.copy(p,v)


