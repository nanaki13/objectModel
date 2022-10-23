package bon.jo.pong

import org.scalajs.dom.HTMLElement
import bon.jo.html.Html.PreDef.*
import bon.jo.html.Html.*
import bon.jo.html.TimeEffect
object DetailsScoreView:

  def roundToSec(timeLeft : Long) =  (timeLeft/1000d).round.toInt
  class RockEffect(endp: End,calcul : HTMLElement,total : HTMLElement,totalScore : HTMLElement,ge : GiftEffect)(using PointCount) extends TimeEffect:
      var p =0
      val interval: Int = 20

      override def onStop(): Option[TimeEffect] = 
        ge.initial = endp.rosckTouch * pointCount.pointsByRock
        Some(ge)
      override def haveToStop(): Boolean =  p == endp.rosckTouch
      override def process(): Unit = 
        p+=1
        calcul.textContent = s": ${ p} * ${pointCount.pointsByRock}"
        total.textContent =( p * pointCount.pointsByRock).toString()
        totalScore.textContent =( p * pointCount.pointsByRock).toString()
  class GiftEffect(endp: End,calcul : HTMLElement,total : HTMLElement,totalScore : HTMLElement,geOpt : Option[TimeCEffect])(using PointCount) extends TimeEffect:
      var p =0
      var initial = 0
      val interval: Int = 20
      override def haveToStop(): Boolean =  p == endp.giftTouch
      override def onStop(): Option[TimeEffect] = geOpt.map{
        ge => 
           ge.initial = initial + endp.giftTouch * pointCount.pointsByGift
           ge
      }
       
      override def process(): Unit = 
        p+=1
        calcul.textContent = s": ${ p} * ${pointCount.pointsByGift}"
        total.textContent =(p * pointCount.pointsByGift).toString()
        totalScore.textContent =( initial + p  * pointCount.pointsByGift).toString()
  class TimeCEffect(endp: GameOver.Victory,calcul : HTMLElement,total : HTMLElement,totalScore : HTMLElement)(using PointCount) extends TimeEffect:
      var p =0
      var initial = 0
      val interval: Int = 20
      override def haveToStop(): Boolean =  p == roundToSec(endp.timeLeft)
      override def process(): Unit = 
        p+=1
        calcul.textContent = s": ${ p} * ${pointCount.pointsBySecond}"
        total.textContent =(p * pointCount.pointsBySecond).toString()
        totalScore.textContent =( initial + p  * pointCount.pointsBySecond).toString()
  def pres(title :HTMLElement,op :HTMLElement,nb : Int,pointByNb : Int) =  
    val totalDiv = Ref[HTMLElement]()
    val ret = div(_class("d-flex fl-ba-1"),childs(title.>(_.classList.add("fl-gr-3")),op.>(_.classList.add("fl-gr-5")),div(text(" = ")).>(_.classList.add("fl-gr-0")),div(bind(totalDiv),text(("0").toString)).>(_.classList.add("fl-gr-4"))))
    //ret.children.foreach(_.asInstanceOf[HTMLElement].classList.add("fl-ba-1"))
    (ret,totalDiv)
  def apply(endp: End): PointCount ?=> (HTMLElement,TimeEffect) =
    val rockCalcul = div(text(s": 0 * ${pointCount.pointsByRock}"))
    val giftCalcul = div(text(s": 0 * ${pointCount.pointsByGift}"))
    val (t1,e1) = div(text(s"rocks")) -> rockCalcul
    
    val (t2,e2)= div(text(s"gifts")) -> giftCalcul
    


    val totalScore = Ref[HTMLElement]()
    val scoreRaw = Seq("".toDiv(_class("fl-gr-3")),"Score".toDiv(_class("ma-ri-1 te-al-end")).wrapDiv(_class("fl-gr-5")), "=" .toDiv(_class("fl-gr-0")), "0".toDiv(bind(totalScore),_class("fl-gr-4"))).wrapDiv(_class("d-flex"))
    scoreRaw.children.foreach(_.asInstanceOf[HTMLElement].classList.add("fl-ba-1"))
    val timeDivOption = endp match
      case v@GameOver.Victory(giftTouch, rosckTouch, timeLeft) =>
        val toSecond = roundToSec(timeLeft)
        val (t3,timeCalcul)= div(text(s"time ")) -> div(text(s": 0 * ${pointCount.pointsBySecond}"))
        val (timePoint,totalTime ) =  (pres(t3,timeCalcul,toSecond,pointCount.pointsBySecond))
        val te = TimeCEffect(endp.asInstanceOf,timeCalcul,totalTime.value,totalScore.value)
        Some(timePoint,totalTime,te)
      case GameOver.Loose(giftTouch, rosckTouch) => None
    val (rockPoint,totalRock ) =  pres(t1,e1,endp.rosckTouch,pointCount.pointsByRock)
    val (giftPoint,totalGift ) =  pres(t2,e2,endp.giftTouch,pointCount.pointsByGift)
    val cont = div(_class("w-15"),childs(rockPoint,giftPoint))

       
    timeDivOption.map(_._1).foreach(cont.:+)
    cont :+ scoreRaw
   
    val ge = GiftEffect(endp,giftCalcul,totalGift.value,totalScore.value,timeDivOption.map(_._3))
    (cont,RockEffect(endp,rockCalcul,totalRock.value,totalScore.value,ge))
   
    
    
