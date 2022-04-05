package bon.jo.home

import org.scalajs.dom.HTMLElement
import scala.reflect.ClassTag
import bon.jo.HtmlPredef.*
import bon.jo.MiniDsl.*
import bon.jo.HtmlEvent.EventAdder
import bon.jo.HtmlEvent.events
import org.scalajs.dom.HTMLHtmlElement
case class PaletteContext(cssSelected: String, cssRow: String, cssCell: String)
object Palette:

  def apply[T](rowSize: Int, ts: T*)(using
      T => HTMLElement,
      PaletteContext
  ): Palette[T] = new Palette(rowSize, ts*)

  def emptyArray[T](rowSize: Int): Array[Option[T]] =
    (for (i <- 0 until rowSize) yield None).toArray
  def rows[T](rowSize: Int, ts: T*): Array[Array[Option[T]]] =
    val rowCount = ts.length / rowSize
    val datas: Array[Array[Option[T]]] =
      (for (i <- 0 until rowCount) yield emptyArray(rowSize)).toArray

    ts.zipWithIndex.map { (e, i) =>
      datas(i / rowSize)(i % rowSize) = Some(e)

    }
    datas

class Palette[T](rowSize: Int, ts: T*)(using
    f: T => HTMLElement,
    p: PaletteContext
):
  var listen : T => Unit = e => ()
  var selected : Option[T] = None
  var allHtmlCell: List[HTMLElement] = Nil
  val rowsp = Palette.rows(rowSize, ts*)
  val childsp = rowsp.map { e =>
    div(
      _class(p.cssRow),
      childs(
        e.filter(_.isDefined).toList.map(_.get).map(v => v -> f(v)).map { (v,h) =>
          h.className = p.cssCell
          h.events.click(_ =>           
              listen(v)
              selected = Some(v)
              allHtmlCell.foreach{
                  _.classList.remove(p.cssSelected)
              }
              h.classList.add(p.cssSelected)
              )
          allHtmlCell = allHtmlCell :+ h
          h
        }
      )
    )
  }
  
  val root = div(childs(childsp))
