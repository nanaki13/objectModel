package bon.jo.sql
trait StringWriter:
  val builder:StringBuilder

object StringWriter:
  type OnStringWriter[A] = StringWriter ?=>A
  type Unit_ = OnStringWriter[Unit]
  type StringWriter_ = OnStringWriter[StringWriter]
  private class Impl(val builder:StringBuilder = StringBuilder()) extends StringWriter

  inline def swBuilder:OnStringWriter[StringBuilder] = summon.builder
  inline def stringWriter: StringWriter_  = summon
  inline def toBuilder(e : Any): Unit_ = swBuilder.append(e)
  inline def /(e : Any): Unit_ =  toBuilder(e)
  inline def apply(ops : OnStringWriter[Unit] *):StringWriter = 
    given StringWriter = Impl()
    ops.foreach{
      op => op
    }
    stringWriter