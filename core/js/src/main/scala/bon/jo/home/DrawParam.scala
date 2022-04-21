package bon.jo.home

import bon.jo.home.ProcessEvent.ActionParam

object DrawParam:
  object Pixel extends DrawParam with ActionParam
  case class Circle(r : Int) extends DrawParam with ActionParam
sealed trait DrawParam extends ActionParam
