package bon.jo.common

final case class SimpleDate(day : Int, month : Int, year : Int) extends Ordered[SimpleDate]:

  override def toString(): String = s"$day/$month/$year"

  def compare(that: SimpleDate): Int = 
    if year > that.year then 
      1
    else 
      if month > that.month then 
        1
      else 
        if day > that.day then 
        1
        else if year < that.year then 
          -1
        else 
          if month < that.month then 
            -1
          else 
            if day < that.day then 
            -1
            else 0

object SimpleDate:
  def apply(s : String):SimpleDate = 
    SimpleDate(s.substring(6).toInt,s.substring(3,5).toInt,s.substring(0,2).toInt)