package bon.jo.account

import scala.scalajs.js.annotation._
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import org.scalajs.dom.console
import bon.jo.common.SimpleDate
import org.scalajs.dom.document
import scala.scalajs.js.Date
import scala.scalajs.js.JSON
import bon.jo.account.highcharttype.SeriesData
import bon.jo.account.highcharttype.ChartOption
import bon.jo.account.highcharttype.DataLabels
import bon.jo.account.highcharttype.Tooltip
import bon.jo.account.highcharttype.Series
import bon.jo.common.JsUtil
import JsUtil.given
import bon.jo.common.FunBuilder.*
import bon.jo.html.Html.*
import bon.jo.html.Html.PreDef.*
import org.scalajs.dom.Element
import org.scalajs.dom.HTMLElement
import bon.jo.account.AccountToOp.ComputedData
import bon.jo.account.AccountToOp.ComputedDataRow
object AccountProcessor {

  extension (e: ComputedDataRow)
    def dateString : String =
      val k = e._1
      val yearmonth = new Date(Date.UTC(k._1, k._2 - 1, 1))
      (yearmonth.getMonth() + 1)
      .toString() + "/" + (yearmonth.getFullYear()).toString()
  def pieChartDebCred(data : ComputedDataRow):ChartOption = 
    val (k,(_, _, debitSumCents, creditSumCents)) = data
    

    val debitSum = debitSumCents / 100d
    val creditSum = creditSumCents / 100d
    val series: Series = |(
      $.name = "balance",
      $.data = js.Array(
        |($.name = "Débit", $.y = -debitSum),
        |($.name = "Crédit", $.y = creditSum)
      )
      
    )
   
    val opts = jsOptNew
    opts.chart.`type` = "pie"
    opts.series = js.Array(series)
    opts.title.text =
      data.dateString + s" Crédit vs Débit : $creditSum - ${-debitSum} = ${(creditSumCents + debitSumCents) / 100d} €"
    given Factory[js.Dynamic] = () => js.Dynamic.literal()
    val plotOptions: js.Dynamic = | {
      $.pie = |[js.Dynamic] {
        $.allowPointSelect = true
        $.cursor = "pointer"
        $.dataLabels = |[js.Dynamic] {
          $.enabled = true
          $.format = "<b>{point.name}</b>: {point.y:.1f} €"
        }
      }
    }
    opts.asInstanceOf[js.Dynamic].plotOptions = plotOptions
    opts
  def globalChartByMonth(computed : ComputedData):ChartOption=
      val globChartByMonthData = computed.map { (k, v) =>
        val yearmonth = new Date(Date.UTC(k._1, k._2 - 1, 1))
        val (debit, credit, debitSumCents, creditSumCents) = v
        val dateString = (yearmonth.getMonth() + 1)
          .toString() + "/" + (yearmonth.getFullYear()).toString()
        |[SeriesData] {
          $.name = dateString
          $.y = (creditSumCents + debitSumCents) / 100d
        }

      }
       val series: Series = |{
        $.name = "crédit - débit"
        $.data = globChartByMonthData.toJSArray
        $.dataLabels = |($.enabled = true)
      }
        
        
      val opts = jsOptNew
      opts.chart.`type` = "column"
      opts.yAxis.title.text = "Euros"
      opts.series = js.Array(series)
      opts

  def optionForChartByOpType(title: String, sd: js.Array[Series]): ChartOption =

    val opts = jsOptNew
    opts.title.text = title

    opts.xAxis.labels.style.fontSize = "20px"
    opts.yAxis.title.text = "Euros"
    opts.series = sd
    opts

  
  def toSeriesByOpType[T](l: js.Array[Operation[T]]): js.Array[Series] =
    l.sortBy(_.value.euroValue)
      .map { e =>
        e.opType -> SeriesData(e.label, e.value.euroValue.abs)
      }
      .groupBy(_._1)
      .map { (t, l) =>
        |[Series](
          $.name = t,
          $.data = l.map(_._2).toJSArray
        )

      }
      .toJSArray
  

  import bon.jo.common.JsUtil.given
  import bon.jo.common.FunBuilder.{$, |}

  def jsOptNew: ChartOption = | {
    $.chart = | {
      $.`type` = "bar"
    }
    $.credits = | {
      $.enabled = false
    }
    $.title = | {
      $.text = ""
    }
    $.subtitle = | {
      $.text = ""
    }
    $.xAxis = | {
      $.`type` = "category"
      $.labels = |($.style = |())
    }
    $.yAxis = | {
      $.title = | { $.text = "" }
      $.labels = |($.style = |())
    }
    $.legend = |()
    $.tooltip = |[Tooltip]($.pointFormat = "<b>{point.y:.1f} €</b>")
    $.series = js.Array(
      |(
        $.dataLabels = |[DataLabels] {
          $.style = |()
        }
      )
    )

  }

  def loadChart(id: String, title: String, sd: js.Array[Series]): Unit =

    val opts = jsOptNew
    opts.title.text = title

    opts.xAxis.labels.style.fontSize = "20px"
    opts.yAxis.title.text = "Euros"
    opts.series = sd
    Highcharts.chart(id, opts)

}
