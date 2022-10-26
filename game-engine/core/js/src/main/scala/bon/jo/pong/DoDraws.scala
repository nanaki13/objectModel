package bon.jo.pong
import bon.jo.common.Geom2D.*
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.console
import org.scalajs.dom.CanvasGradient
 

trait DoDraws extends Ctx[CanvasRenderingContext2D]: 
  self : Drawer[CanvasRenderingContext2D] =>

    inline def saveAndRestore(inline f : => Unit): CanvasRenderingContext2D ?=> Unit =
      ctx.save()
      f
      ctx.restore()
    given DoDraw[CanvasRenderingContext2D, Gift] with
      def apply(gift: Gift): CanvasRenderingContext2D ?=> Unit =
          ctx.beginPath()
          gradient(ctx, gift,"#3fc73b","#0da853","#ba1b92")
          gift.valuep.fill()
          gift.drawDef()

 
    given DoDraw[CanvasRenderingContext2D, Board] with
      def apply(board: Board): CanvasRenderingContext2D ?=> Unit =
      
          
          board.paths.foreach { p =>
            ctx.beginPath()
            gradient(ctx, board)
            p.fill()

          }
          board.drawDef()
        

    given DoDraw[CanvasRenderingContext2D, Rock] with

      def apply(s: Rock): CanvasRenderingContext2D ?=> Unit =
          ctx.beginPath()
          val pO = Point(s.value.from.x, s.value.from.y)
          ctx.moveTo(s.value.from.x, s.value.from.y)
          val pfin = s.value.elements.foldLeft(pO)((pp, v) => {
            val p = pp + v
            ctx.lineTo(p.x, p.y)
            p
          })  
        
          ctx.fillStyle = s.color
          gradient(ctx, s,"#96379d", "pink",s.color)
          ctx.fill()
          s.drawDef()

     
       
    def gradient(ctx: CanvasRenderingContext2D, r: Shape[_],c1 : String = "#96379d",c2 : String = "pink",c3 : String = "#96379d"): Unit =
      val gr = ctx.createLinearGradient(
        r.pos.x,
        r.pos.y,
        r.pos.x + r.valuep.w,
        r.pos.y + r.valuep.h
      )
      gr.addColorStop(0.1, c1)
      gr.addColorStop(0.5, c2)
      gr.addColorStop(0.7, c3)


      ctx.fillStyle = gr
    given DoDraw[CanvasRenderingContext2D, Player] with
      def apply(r: Player): CanvasRenderingContext2D ?=> Unit =
          val gr =
            ctx.createLinearGradient(r.pos.x, r.pos.y, r.pos.x + r.path.w, r.pos.y)
          gr.addColorStop(0.1, "#96379d")
          gr.addColorStop(0.2, "pink")
          gr.addColorStop(0.4, "#96379d")
          ctx.fillStyle = gr
          ctx.beginPath()
          r.path.fill()
          r.drawDef()  

