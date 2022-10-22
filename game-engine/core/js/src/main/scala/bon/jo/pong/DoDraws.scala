package bon.jo.pong
import bon.jo.Geom2D.*
import org.scalajs.dom.CanvasRenderingContext2D

trait DoDraws extends Ctx[CanvasRenderingContext2D] : 
  self : Drawer[CanvasRenderingContext2D] =>
    given DoDraw[CanvasRenderingContext2D, Gift] with
      def apply(board: Gift): CanvasRenderingContext2D ?=> Unit =
        gradient(ctx, board)
        ctx.beginPath()
        board.valuep.fill()
        board.drawDef()
    given DoDraw[CanvasRenderingContext2D, Board] with
      def apply(board: Board): CanvasRenderingContext2D ?=> Unit =
        gradient(ctx, board)
        board.paths.foreach { p =>
          ctx.beginPath()
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
        val o = ctx.fillStyle
        ctx.fillStyle = s.color
        gradient(ctx, s)
        ctx.fill()
        ctx.fillStyle = o
        s.drawDef()
    def gradient(ctx: CanvasRenderingContext2D, r: Shape[_]): Unit =
      val gr = ctx.createLinearGradient(
        r.pos.x,
        r.pos.y,
        r.pos.x + r.valuep.w,
        r.pos.y + r.valuep.h
      )
      gr.addColorStop(0.1, "#96379d")
      gr.addColorStop(0.5, "pink")
      r match
        case e: Rock => gr.addColorStop(0.7, e.color)
        case _       => gr.addColorStop(0.7, "#96379d")

      ctx.fillStyle = gr
    given DoDraw[CanvasRenderingContext2D, Player] with
      def apply(r: Player): CanvasRenderingContext2D ?=> Unit =
        ctx.beginPath()
        val gr =
          ctx.createLinearGradient(r.pos.x, r.pos.y, r.pos.x + r.path.w, r.pos.y)
        gr.addColorStop(0.1, "#96379d")
        gr.addColorStop(0.2, "pink")
        gr.addColorStop(0.4, "#96379d")
        val o = ctx.fillStyle
        ctx.fillStyle = gr
        r.path.fill()
        ctx.fillStyle = o
        r.drawDef()