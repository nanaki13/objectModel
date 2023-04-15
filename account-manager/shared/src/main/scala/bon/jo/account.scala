package bon.jo:
  package account:
    import math.Ordered.orderingToOrdered
    opaque type Cents = Int
    extension (e : Int)
      def cents : Cents = e
    extension (o : Cents)
      def euroValue : Double = o/100d
      inline def value : Int = o 

    case class Operation[T](value : Cents,date : T,label : String,opType : String)
    case class ValuedOperation[T](amountAfter : Cents,operation : Operation[T])
    case class Operations[T](operations : Seq[Operation[T]])
    case class ValuedOperations[T](operations : Seq[ValuedOperation[T]] = Nil)
    case class Account(amount : Int)
    extension [T] (o : Operation[T])
      def euroValue : Double = o.value.euroValue
      def prettyString : String = s"${o.date} ${o.value.euroValue} : ${o.label.replaceAll("(\r|\n)","-")}"
    extension [T] (o : Operations[T])
      
      def calculate(from : T,startValue : Int)(using Ordering[T]):ValuedOperations[T] =
        val (before, after) =  o.operations.partition(_.date <= from)
        val afterComputed = after.foldLeft(ValuedOperations(Seq(ValuedOperation(startValue,Operation(0,from,"init",""))))){
          (tot,op) => {
            tot.copy(tot.operations :+ ValuedOperation( tot.operations.last.amountAfter.value + op.value.value,op))
          }
        }
        val beforeComputed = before.reverse.foldLeft(ValuedOperations(afterComputed.operations.drop(1).head :: Nil)){
          (tot,op) => {
            tot.copy(tot.operations :+ValuedOperation(tot.operations.last.amountAfter.value - tot.operations.last.operation.value.value ,op))
          } 
        }
        ValuedOperations(beforeComputed.operations.reverse++afterComputed.operations.drop(1))


    object Main:
      @main
      def test():Unit = 
        val ops = for{ 
          i <- 1 to 10
          ops = Operation(i.cents,i,i.toString(),i.toString())
          } yield ops
        val opso = Operations(ops)
        println(opso)
        println(opso.calculate(4,100))
        