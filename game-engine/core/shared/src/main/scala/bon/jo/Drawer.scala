package bon.jo.pong

import bon.jo.pong.Board
import bon.jo.pong.PongSystem
import bon.jo.pong.Ball
import bon.jo.Geom2D.Segment

trait Drawer[C] {
    type CanvasDraw[A] = C ?=> A
    def clear(b : Board):CanvasDraw[Unit]
    inline def ctx : CanvasDraw[C] = summon
    extension (p : PongSystem)
      def draw() :CanvasDraw[Unit] = 

        clear(p.board)
        p.board.draw()
        p.ball.draw()
        p.player.foreach(_.draw())
    extension (board : Board)
      def draw() :CanvasDraw[Unit] = 
        board.paths.foreach{
          p => 
            p.segments.foreach{ s =>
              s.draw()
            }
        }
    extension (ball : Ball)
      def draw() :CanvasDraw[Unit] 
    extension (ball : Segment)
      def draw() :CanvasDraw[Unit] 
    extension (player : Player)
      def draw() :CanvasDraw[Unit] 
}
