package bon.jo.utils

trait ConvertList[I,O]:
  def done : List[O]
  def current : InputsToOutput[I,O] 
  def allDone = 
    if current.buff.nonEmpty
    then
      done :+ current.build
    else
        done

object ConvertList:
  case class Impl[I,O](done : List[O],current : InputsToOutput[I,O]) extends  ConvertList[I,O]
  
  def apply[I,O](l : List[I])(using builder : InputsToOutput[I,O]):List[O]=
    val first = Impl(Nil,builder)
    val aggf = l.foldLeft(first)((agg,el) => {
      if(agg.current.accept(el)) then agg.copy(current = agg.current.copy(agg.current.buff :+ el))
      else 
        val done = agg.allDone

        agg.copy(done =done:+ agg.current.refuse(el),current = agg.current.copy(buff = Nil))
    })
    
    aggf.allDone

