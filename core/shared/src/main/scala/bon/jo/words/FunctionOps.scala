package bon.jo.words

import bon.jo.words.MathExp.FunctionMathExp
import bon.jo.words.MathExp.Function
trait FunctionOps:
  self : Function =>
    
    def evaluate( p :Map[PhraseElement,MathExp.Number]) = 
      MathExp.Number( MathExp.stringFunction(value)(param.tryApply(p)),null)
    def toFunctionMathExp : FunctionMathExp = 
      FunctionMathExp(fun = p =>evaluate(p),source)
    
    def isNumber : Boolean = false
    def isOperator : Boolean = false
    def isFunction : Boolean = true