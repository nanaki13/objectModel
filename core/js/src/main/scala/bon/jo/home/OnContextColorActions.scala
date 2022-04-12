package bon.jo.home

import org.scalajs.dom.HTMLElement
import bon.jo.home.GridViewContext
import bon.jo.home.GridViewContext.*
import bon.jo.home.Color
import bon.jo.Draw.EmptyGridElement
import bon.jo.HtmlPredef.*
import bon.jo.MiniDsl.*
import bon.jo.HtmlEvent.EventAdder
trait OnContextColorActions:
  def saveColor(out: HTMLElement): OnContextUnit =
    val saved = context.color
    saveColor(saved, out)
  inline def tillColor() = div(_class("color-till"))
  def colorFromDraw(outPallette: HTMLElement): OnContextUnit =
    val savedSet = context.savedColor.toSet
    (context.grid.data ++ context.grid.sheet.flatMap(_.v.data))
      .filter(_ != EmptyGridElement)
      .map(_.asGridValue[String]().v)
      .toSet
      .map(Color.Raw.apply)
      .filter(!savedSet.contains(_))
      .foreach(saveColor(_, outPallette))
    
  def updateColor(c: Color): OnContextUnit =
    context.color = c
    context.colorPicker.style.backgroundColor = c.toString
  def saveColor(saved: Color, out: HTMLElement): OnContextUnit =
    context.savedColor = context.savedColor :+ saved
    val d = tillColor()
    out.append(d)
    d.style.backgroundColor = saved.toString
    EventAdder.click(_ => updateColor(saved))(using d)
