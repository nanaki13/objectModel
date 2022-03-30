package bon.jo.objects

import bon.jo.objects.StringExctractor.add
import scala.collection.mutable
import StringExctractor.*
private trait StringExctractor[T](using compat : Compat[T]):
 
  inline def add(c : CharPos):StringExctractor[T ] = 
    if builder == EmtyBuilder then
      Result(builded,builder = compat.create(c)) 
    else if compatible(c) then
      Result(builded,builder =builder.add(c)) 
    else 
      Result(builded =builded :+ builder.build  ,compat.create(c)) 

  def builded:List[ T]
  def builder:Builder[ T]

  inline def compatible(c : CharPos): Boolean = 
   builder.compatible(c )
object StringExctractor:
  
  trait Compat[T]:
    def add(t :  Builder[T],c : CharPos): Builder[T]
    def create(c : CharPos): Builder[T]
  case class Result[T](builded : List[T] = Nil,builder : Builder[T] = EmtyBuilder )(using compat : Compat[T])  extends StringExctractor[T]
  def parse[T](s : String)(using  compat : Compat[T]):List[T] = 
    if s.nonEmpty then
      val se:StringExctractor[T] = new Result[T](builder = EmtyBuilder)
      val sAfter = s.zipWithIndex.map(CharPos.apply).foldLeft(se)((seref,char) => seref.add(char))
      ( sAfter.builded :+ sAfter.builder.build  )
    else
      Nil
 
  case class CharPos( current:Char,pos:Int)

  trait Builder[+T]:
    val value  :StringBuilder 
    def build : T
    def pos : Int
    def compatible(c : CharPos): Boolean
  val empty : Builder[Nothing]= new:
    val value  :StringBuilder  = null
    def build : Nothing = ???
    val pos : Int = 0
    def compatible(c : CharPos): Boolean = false 
  def EmtyBuilder[T] : Builder[T]= empty


  extension [T](t : Builder[T])
    inline def add(c : CharPos)(using comp : Compat[T]): Builder[T] = comp.add(t,c)
  

  
