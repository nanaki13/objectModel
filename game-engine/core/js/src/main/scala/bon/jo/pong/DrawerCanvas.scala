package bon.jo.pong
import org.scalajs.dom.CanvasRenderingContext2D
import bon.jo.Geom2D.*
object DrawerCanvas extends Drawer[CanvasRenderingContext2D] with DoDraws:

  def clear(board: Board): CanvasDraw[Unit] =
    ctx.clearRect(0, 0, board.w * 2, board.h * 2)

  def circle(pos: Point, r: Double): CanvasDraw[Unit] =

    ctx.ellipse(
      x = pos.x,
      y = pos.y,
      radiusX = r,
      radiusY = r,
      rotation = 0,
      startAngle = 0,
      endAngle = 2 * Math.PI
    )
    ctx.fill()

  extension (s: Segment)
    def draw(): CanvasDraw[Unit] =
      ctx.beginPath()
      ctx.moveTo(s.p1.x, s.p1.y)
      ctx.lineTo(s.p2.x, s.p2.y)
      ctx.stroke()
  extension (s: Path)
    def fill(): CanvasDraw[Unit] =

      ctx.moveTo(s.from.x, s.from.y)
      var pc = s.from
      for (p <- s.elements)
        pc = pc + p
        ctx.lineTo(pc.x, pc.y)
      ctx.fill()
  // val ballImage = <.img[HTMLImageElement].>(_.src="https://img.lovepik.com/element/45007/2796.png_300.png")

  
  extension (ball: Ball)
    def draw(): CanvasDraw[Unit] =

        val pos = ball.pos
        val gradient = ctx.createRadialGradient(
          pos.x,
          pos.y,
          ball.shape.r / 10,
          pos.x,
          pos.y,
          ball.shape.r
        );
        val r = ball.shape.r
  // Add three color stops
        gradient.addColorStop(0, "#ff5900")
        gradient.addColorStop(0.5, "#07fff8")
        gradient.addColorStop(1, "green")

  // Set the fill style and draw a rectangle
        ctx.beginPath()
        ctx.fillStyle = gradient
        circle(pos, ball.shape.r)


