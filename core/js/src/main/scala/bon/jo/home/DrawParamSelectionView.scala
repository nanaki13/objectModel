package bon.jo.home

import bon.jo.home.ProcessEvent.ActionParam
import bon.jo.MiniDsl.*
import bon.jo.HtmlPredef.*
import bon.jo.HtmlEvent.*
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLInputElement

object DrawParamSelectionView:
  def apply(listen : DrawParam => Unit):HTMLElement = 
    val pointInputCheck = inputCheck(true)
    val circleInputCheck = inputCheck(false)
    val rInput = input
    var last : Option[HTMLInputElement] = Some(pointInputCheck)
    pointInputCheck.events.change{
      _ => 
        println(pointInputCheck.checked)
        if pointInputCheck.checked then
          org.scalajs.dom.console.log(last)
          last.foreach(_.checked = false)
          last = Some(pointInputCheck)
          listen(DrawParam.Pixel)
        else
          pointInputCheck.checked =true
          
    }
    circleInputCheck.events.change{
      _ =>   
        if circleInputCheck.checked   then      
          last.foreach(_.checked = false)
          last = Some(circleInputCheck)
          listen(DrawParam.Circle(rInput.value.toInt))
        else 
          circleInputCheck.checked = true
          
    }
    rInput.events.change{ _ =>  
      if circleInputCheck.checked   then      
        listen(DrawParam.Circle(rInput.value.toInt))
    }
    val d1 =  div(childs(pointInputCheck,span(_text("Pixel"))))
    val d2 =  div(childs(circleInputCheck,span(_text("Circle")),span(_text("R : ")),rInput))
    div(childs(d1,d2))