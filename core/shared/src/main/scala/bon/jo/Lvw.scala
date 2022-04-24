package bon.jo

case class Lvw(val symbols : String,val datas : Iterable[Int],val dicoSize : Int):

  def dataSize(v : Float,current : Int):Int = 
    if v > 16 then 
      dataSize(v/16,current+1)
    else
      current
  def toStringData : String =     
    val ds = dataSize(dicoSize,1)

    symbols.size.toHexString.reverse.padTo(4,'0').reverse +symbols+ds.toHexString.reverse.padTo(4,'0').reverse +datas.map(e => 
      e.toHexString.reverse.padTo(ds,'0').reverse  
    ).mkString
object Lvw :

  case class LvwMetaData(sybSize : Int,dicoIni: Map[Int, String], dataBlockSize: Int, dataStartIndex: Int)
  object LvwMetaData:
    def unapply(s : Iterator[Char]):Some[( Int, Map[Int, String], Int,  Int)] =
      val sybSize  = readInt(s)
      val dicoIni = (for( i <- 4 until 4+sybSize ) yield s.next).map(_.toString).zipWithIndex.map(_.swap).toMap
      val mutableDico = scala.collection.mutable.Map(dicoIni.toSeq *)
      val dataSize = readInt(s)
      val dataStartIndex = 8+sybSize 
      Some((sybSize,dicoIni,dataSize,dataStartIndex))
  def readInt(s : String,from:Int):Int = Integer.parseInt( s.substring(from, from +4),16 )
  def readInt(s : String,from:Int,size : Int):Int = Integer.parseInt( s.substring(from, from +size),16 )
  def readInt(it : Iterator[Char]):Int = 

    Integer.parseInt( it.next.toString+it.next+it.next+it.next ,16 )
  def readInt(it : Iterator[Char],size : Int):Int = 
  
    val buff = StringBuilder()
    for{
      i <- 0 until size
      read = it.next.toString
    }  {buff.append(read)}
  
    
    Integer.parseInt( buff.toString ,16 ) 
  inline def unapply(ss : String):Some[String] = unapply(ss :Iterable[Char] )
  def unapply(ss : Iterable[Char]):Some[String] = 
    val it = ss.iterator
    val LvwMetaData(sybSize : Int,dicoIni: Map[Int, String], dataBlockSize: Int, dataStartIndex: Int) =  it 
    val mutableDico = scala.collection.mutable.Map(dicoIni.toSeq *)  
    var previous : Option[String] = None
    val all = StringBuilder()
    try
      while (it.hasNext) {
        val code = readInt(it,dataBlockSize)
        //println(code)
    

        val current = if mutableDico.contains(code) then
          val c = mutableDico(code)
          previous.foreach{
            p => 
              mutableDico(mutableDico.keys.max +1) = p+c.head
          }
          c
        else
          val c = previous.get+previous.get.head
          mutableDico(code) = c
          c
          
        previous = Some(current)
        all.append(current)
      }
    catch 
      case e => e.printStackTrace()
    Some(all.toString)

  def apply(s :  Iterable[Char]):Lvw = apply(()=>s) 
  def apply(s : ()=> Iterable[Char]):Lvw = 
    val org = s().toSet.map(_.toString).zipWithIndex.toMap
    val dico : scala.collection.mutable.Map[String, Int]= scala.collection.mutable.Map(org.toSeq *)
    var i = 0
    var current = StringBuilder("")
   
    val comp = (for(c <- s()) yield

      val p = current.clone().append(c)
     
    
      if(dico.contains(p.toString)) then {
        current = StringBuilder(p.toString)   
        None   
      } else
        dico(p.toString) = dico.values.maxOption.getOrElse(0)+1
        val ret = dico(current.toString)
       
        current = StringBuilder(c.toString)
        Some(ret)).flatten ++ Some(dico(current.toString))
    new Lvw(org.keySet.mkString,comp,dico.size)
