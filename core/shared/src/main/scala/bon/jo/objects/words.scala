package bon.jo.objects

import scala.collection.mutable
import bon.jo.words.PhraseElement
import bon.jo.words.PhraseElement.PhraseElementBuilder
import bon.jo.words
import bon.jo.objects.StringExctractor.{Compat,CharPos,Builder}
import bon.jo.words.Phrase.removeSpace
import scala.annotation.tailrec
import bon.jo.utils.InputsToOutput
import bon.jo.utils.ConvertList
import bon.jo.utils.Tree
import bon.jo.utils.Tree.EndTree
import bon.jo.words.PhraseElement.StringEscape
import bon.jo.words.PhraseElement.NumberReduce
import bon.jo.words.MathExp
import bon.jo.words.MathExp.FunctionMathExp

object Main:

  //@main
  def test() : Unit = 

    var mathTree : Tree[MathExp] = MathExp("|")
    println(mathTree)
    val mt = mathTree.compile()
    println(mt)
    println("--------------")

    for( i <- 0 to 10)
      println(mt.tryApplyMap(Map("x"->i,"a"->i*Math.PI/2,"pi"->Math.PI,"y"->i*Math.PI/2)))
  // println(mathTree.compile())
  // val t = mathTree.compile().tryApplyMap(Map("x"->2,"y"->2))
  //  println(t)
