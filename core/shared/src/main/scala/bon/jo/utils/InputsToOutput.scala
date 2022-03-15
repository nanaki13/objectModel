package bon.jo.utils

trait InputsToOutput[I,O]:
  def build : O
  def buff : List[I]
  def accept(e :I): Boolean
  def copy(buff : List[I]): InputsToOutput[I,O]
  def refuse(e : I):O
object InputsToOutput:
  case class Impl[I,O](buff : List[I],bf : List[I] => O,acceptF : (List[I],I) => Boolean,refuseF : I => O) extends InputsToOutput[I,O]:
    def build : O =bf(buff)
    def copy(buff : List[I]): InputsToOutput[I,O] = this.copy(buff,bf)
    def accept(e :I): Boolean = acceptF(buff,e)
    def refuse(e : I):O = refuseF(e)
    
  def apply[I,O](bf : List[I] => O,acceptF : (List[I],I) => Boolean,refuse : I => O) : InputsToOutput[I,O] = Impl(Nil,bf,acceptF,refuse)