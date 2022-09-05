package bon.jo

import scala.annotation.tailrec
import bon.jo.System.*

trait System:
 // def elements:Seq[SystemElement]
  def nextSystem[  T <: System ]():PartialFlow[T]  = 
    given T = this.asInstanceOf[T]
    System.nextSystem()


object System:
  type S[R] = R ?=> R
  type SystemFlow[R <: System] = S[R]
  type Sys[S<: System,A] = S ?=> A
  type PartialFlow[S<: System] = (SystemProcess[S]) ?=> S
  type ProcessFlow[S<: System] = (S,SystemProcess[S]) ?=> S
  def apply[T <: System](els : List[SystemElement])(using List[SystemElement] => T):T = summon(els)
  inline def apply[T <: System]():SystemFlow[T] = summon
  def nextSystem[S <: System]():ProcessFlow[S] = 
     SystemElementProcess().next()

  trait SystemElement
  object SystemElementProcess:
    inline def apply[S <: System]():SystemProcess[S] ?=> SystemProcess[S]  = summon
  trait SystemProcess[S <: System]:
    def next():SystemFlow[S]

    

