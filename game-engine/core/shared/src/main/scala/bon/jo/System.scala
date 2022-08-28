package bon.jo

import scala.annotation.tailrec

type S[R] = R ?=> R
type SystemFlow[R <: System] = S[R]
type PartialFlow[S<: System] = (SystemElementProcess[S], List[SystemElement] => S) ?=> S
type ProcessFlow[S<: System] = (S,SystemElementProcess[S], List[SystemElement] => S) ?=> S
type ElementFlow[S<: System] = S ?=> SystemElement
trait System:
  def elements:List[SystemElement]
  def nextSystem[  T <: System ]():PartialFlow[T]  = 
    given T = this.asInstanceOf[T]
    System.nextSystem()


object System:
  def apply[T <: System](els : List[SystemElement])(using List[SystemElement] => T):T = summon(els)
  inline def apply[T <: System]():SystemFlow[T] = summon
  def nextSystem[S <: System]():ProcessFlow[S] = 
     System(System().elements.map(e => SystemElementProcess().next(e)))

trait SystemElement
object SystemElementProcess:
  inline def apply[S <: System]():SystemElementProcess[S] ?=> SystemElementProcess[S]  = summon
trait SystemElementProcess[S <: System]:
 
  def next(s :  SystemElement):ElementFlow[S]

    

