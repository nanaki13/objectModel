package bon.jo.common
import Anys.*
import bon.jo.common.Strings.FileExtension
object Strings {
  
  object FileExtension:
    def unapply(s : String):Option[(  String,  String)] = 
      val dotindex = s.lastIndexOf('.')
      if dotindex != -1 then
        (s.substring(0,dotindex),s.substring(dotindex+1)).toSome
      else None
}
