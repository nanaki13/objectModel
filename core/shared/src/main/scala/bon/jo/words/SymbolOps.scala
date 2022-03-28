package bon.jo.words

import bon.jo.words.MathExp.FunctionMathExp
import bon.jo.words.MathExp.Function
import bon.jo.words.MathExp.Symbol
trait SymbolOps:
  self : Symbol =>
    def asFunction() : FunctionMathExp = FunctionMathExp(Set(source.value),_(source.value),source)
    def isNumber : Boolean = false
    def isOperator : Boolean = false
    def evaluate(p : Map[PhraseElement,MathExp.Number]):MathExp.Number= p(this.source.value)
    def isFunction : Boolean = false
    override def isSymbol: Boolean = true