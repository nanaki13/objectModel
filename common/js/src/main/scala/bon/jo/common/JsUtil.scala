package bon.jo.common
import scalajs.js
import scalajs.js.UndefOr
import bon.jo.common.FunBuilder.Factory
import bon.jo.common.FunBuilder.Building
object JsUtil {
  def apply[T <: js.Object]():T = js.Object().asInstanceOf[T]

  given [T<: js.Object] : Factory[T] = () => apply()
  given [T<: js.Object] : Factory[UndefOr[T]] = () => apply()

   
 
}
