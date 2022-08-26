package bon.jo

import scala.annotation.tailrec

type S[R] = System ?=> R
type SystemFlow = S[System]
type ElementFow = (System,SystemElementProcess) ?=> System
trait System:
  def elements:Iterable[SystemElement]
  def nextSystem():SystemElementProcess ?=> System = 
    given System = this
    System.nextSystem()
  def update[T <: System](old : SystemElement,newE : SystemElement)(using List[SystemElement] => T):T = 
    System(elements.map{
        ee => 
          if ee == old then
            newE
          else
            ee
      }.toList)

object System:
  def apply[T <: System](els : List[SystemElement])(using List[SystemElement] => T):T = summon(els)
  inline def apply():SystemFlow = summon
  def nextSystem():ElementFow = 
    process(System().elements.toList,System())
  @tailrec
  private def process(toDo : List[SystemElement], onGoing : System):  ElementFow = 
    if toDo.isEmpty then
      onGoing
    else 
      process(toDo.tail,SystemElementProcess().next(toDo.head.asInstanceOf,onGoing))
trait SystemElement
object SystemElementProcess:
  inline def apply():SystemElementProcess ?=> SystemElementProcess = summon
trait SystemElementProcess:
  type T <: SystemElement
  def next(s :  T, onGoing : System):SystemFlow

    

