package bon.jo.pong

import bon.jo.service.ScoreService
import org.scalajs.dom.HTMLElement
import bon.jo.html.Html.*
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
object TopScoreView:

  def view(using ser: ScoreService):Future[HTMLElement] = 
    ser.getScores().map{ 
      scores => 
        val trs = scores.map{
          score =>  <.td[HTMLElement](childs(<.tr[HTMLElement](text(score.owner)),<.tr[HTMLElement](text(score.value.toString()))))
        }
        <.table[HTMLElement](childs(trs *))    
    }
