package bon.jo.words

import bon.jo.words.PhraseElement.PositionedPhraseElement
import bon.jo.words.PhraseElement.PosPe
import bon.jo.words.PhraseElement.StringEscape
import bon.jo.words.PhraseElement.NumberReduce
import bon.jo.utils.Tree
import bon.jo.utils.Tree.Node
import bon.jo.utils.Tree.EndTree


case class Phrase(elements : List[PosPe]):
  inline def +(e  :PosPe) = copy(elements :+ e)
object Phrase:
  type OptionPosPe = List[PosPe]
  case class ParsingException(val message : String,val element : OptionPosPe) extends RuntimeException(message):
    def this(message : String,element_ :PosPe ) =  this(message,List(element_)) 
    def this(message : String,element_ :PosPe,element_2 :PosPe ) =  this(message,List(element_,element_2)) 
    def this(message : String) = this(message,Nil) 
  case class NodeLevel(node : Node[PosPe],level : Int = 0)
  extension (e : List[PosPe])
    def numberFormat(): List[PosPe] = NumberReduce(e)
    def removeWhite(): List[PosPe] = StringEscape(e)
    def toTree(open : PhraseElement.Symbol,close : PhraseElement.Symbol):Node[PosPe] = 
      val ret = e.removeSpace().foldLeft(NodeLevel(Tree.Node[PosPe](Nil)))((nodeLevel,phel) => {
        val node = nodeLevel.node
        if phel.value == open then
            nodeLevel.copy(Tree.Node(Nil,node),nodeLevel.level+1) 
        else if phel.value == close then
            if(nodeLevel.level == 0) then
              throw new ParsingException("close before open",phel)
            nodeLevel.copy(node.parent.copy(childs = node.parent.childs :+node ),nodeLevel.level-1) 
        else
            val (endTree : EndTree[PosPe],isNew: Boolean)= node.childs.lastOption match
              case Some(v : EndTree[PosPe]) => v -> false
              case _ => Tree.EndTree(Nil,node) -> true
            val nEtry = endTree.copy(values = endTree.values :+ phel)
            val nChilds = if isNew then node.childs :+ nEtry else node.childs.dropRight(1):+nEtry
            nodeLevel.copy(node.copy(nChilds))
      

      })
      if ret.level != 0 then throw new ParsingException(s"open but not close ${ret.level} times")
      ret.node
    def removeSpace() = e.filter{
      case PositionedPhraseElement(_, PhraseElement.Space(_) )=> false
      case _ => true
    }

  // def apply(str : String):Phrase = 
end Phrase