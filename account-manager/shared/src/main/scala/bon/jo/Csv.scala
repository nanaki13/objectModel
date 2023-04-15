package bon.jo

import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.Conversion
import scala.util.Try
import bon.jo.account.*
import bon.jo.common.SimpleDate

object Csv {

  @main
  def testCsv(): Unit =



    given Param = Param(';', '"')
    println(parseProcess("a;b;c\nd;e;f\n".iterator).parse(println))
    val s = Source.fromFile("test.csv", "utf8")

    given Conversion[Row, Operation[SimpleDate]] =
      e =>
        val date = e(0)
        val label = e(1)  
        val amount =
          (if e(2).isEmpty then (e(3).toDoubleForce * 100).toInt
           else -(e(2).toDoubleForce * 100).toInt)
        Operation(amount.cents, SimpleDate(date), label,"")
    val ops = rowTo(s.iterator)
    
    s.close()
    val (debit,credit) = ops.partition(_.value.value>0)
    println(debit.toSeq.sortBy(_.value.euroValue).map(_.prettyString).mkString("\n"))
    println(debit.map(_.value.value).sum.euroValue)
    println(credit.toSeq.sortBy(_.value.euroValue).map(_.prettyString).mkString("\n"))
    println(credit.map(_.value.value).sum.euroValue)
  def isDigitOrSep(z : Char) = z.isDigit || z == ','|| z == '.'
  extension (ss: String)
    def toDoubleForce: Double =
      
      val s = ss.filter(isDigitOrSep)
      Try(s.toDouble).recoverWith { _ =>
        Try(s.replace(",", ".").toDouble) recoverWith { _ =>
          Try(s.replace(".", ",").toDouble)
        }
      }.get
  def parseProcess(str: Iterator[Char])(using p: Param): ParseProcess =
    fr =>
      val ctxEnd = str
          .foldLeft(Context()) { (ctx, ch) =>
            if ctx.state == State.EndLine then

              if(ctx.result.row.nonEmpty)
                fr(ctx.result)
              if ch != '\n' && ch != '\r' then
                Context().add(ch)
              else
                Context()
            else ctx.add(ch)
          }
      if(ctxEnd.result.row.nonEmpty)
                fr(ctxEnd.result)

  def row(str: Iterator[Char])(using p: Param): Seq[Row] =
    val ret = ListBuffer[Row]()
    parseProcess(str).parse(ret.addOne)
    ret.toList
  def rowTo[T](str: Iterator[Char])(using Param, Conversion[Row, T]): Seq[T] =
    val ret = ListBuffer[T]()
    parseProcess(str).parseTyped[T]( ret.addOne)
    ret.toList

  case class Param(sep: Char, stringSep: Char)
  case class Context(
      previous: Char = (-1).toChar,
      buffer: StringBuilder = StringBuilder(),
      state: State = State.Normal,
      result: Row = Row()
  )
  case class Row(row: Seq[String] = Nil):
    def apply(i: Int): String = row(i)
  enum State:
    case Normal, InString, EndLine
  trait ParseProcess:
    def parse(f: Row => Unit): Unit
    def parseTyped[T](f: T => Unit)(using Conversion[Row, T]) =
      parse(e => f(e.convert))
  extension (con: Context)
    def addChar(c: Char): Context = con.copy(c, con.buffer.append(c))
    def addValue(c: Char): Context =
      con.copy(
        previous = c,
        buffer = mutable.StringBuilder(),
        result = con.result.copy(con.result.row :+ con.buffer.toString())
      )

    def add(c: Char)(using p: Param): Context =
      con.state match
        case State.Normal =>
          c match
            case p.stringSep =>
              con.copy(previous = c, state = State.InString)
            case '\n' | '\r' =>
              con.addValue(c).copy(state = State.EndLine)
            case p.sep =>
              con.addValue(c)
            case o =>
              con.addChar(o)
        case State.InString =>
          if con.previous == '\\' then
            c match
              case p.stringSep => con.addChar(c)
              case o           => con.copy(c, con.buffer.append(f"\\" + o))
          else
            c match
              case p.stringSep =>
                con.copy(previous = c, state = State.Normal)
              case o => con.addChar(o)
        case State.EndLine => con

}
