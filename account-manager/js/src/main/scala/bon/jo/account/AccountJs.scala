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
import bon.jo.account.AccountProcessor.dateString
@JSExportTopLevel("Account")
@JSExportAll
object AccountJs :

  
  def createContForChart(id: String): HTMLElement =
    val divChart :Ref[HTMLElement] = Ref()
    (<.figure[HTMLElement]{
      _class("highcharts-figure")
      childs(div(bind(divChart),*.id=id))
    }) --> document.body
    divChart.value

  def load(data: js.Array[AccountDto]): Unit = {

    val computed = AccountToOp(data)

      
      
    val opts = AccountProcessor.globalChartByMonth(computed)
        
    Highcharts.chart(
      createContForChart("container_by_month"),
      opts
    )
    computed.foreach { e =>
     
      val opts =  AccountProcessor.pieChartDebCred(e)
      val k = e._1
      val (debit, credit, _, _) = e._2
      Highcharts.chart(
        createContForChart("container_debit__cred" + k._1 + "_" + k._2),
        opts
      )
      val dataDebit = AccountProcessor.toSeriesByOpType(debit)
      val dataCredit = AccountProcessor.toSeriesByOpType(credit)
      val divChart = createContForChart("container_debit_" + k._1 + "_" + k._2)
      val divChart2 =
        createContForChart("container_credit_" + k._1 + "_" + k._2)
      Highcharts.chart( createContForChart("container_debit_" + k._1 + "_" + k._2),AccountProcessor.optionForChartByOpType(e.dateString+ " Debit",dataDebit.toJSArray))
      Highcharts.chart( createContForChart("container_credit_" + k._1 + "_" + k._2),AccountProcessor.optionForChartByOpType(e.dateString + " Credit",dataCredit.toJSArray))

    }

  }

  



