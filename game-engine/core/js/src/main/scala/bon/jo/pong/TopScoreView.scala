package bon.jo.pong

import bon.jo.service.ScoreService
import org.scalajs.dom.HTMLElement
import scalajs.js.Date
import bon.jo.html.Html.*
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
object TopScoreView:

  def view(using ser: ScoreService): Future[HTMLElement] =
    ser.getScores().map { scores =>
      println(scores)
      val trs = scores.map { score =>
        val dateScore =  new Date(score.score.scoreDateTime)
        
        <.tr[HTMLElement](
          childs(
            <.td[HTMLElement](text(score.user.name),_class("table-top-score-user")),
            <.td[HTMLElement](text(score.score.scoreValue.toString()),_class("table-top-score-value")),
            <.td[HTMLElement](
              text(dateScore.toLocaleString()),_class("table-top-score-date")
            )
          )
        )
      }
      <.table[HTMLElement](childs(trs*))
    }
