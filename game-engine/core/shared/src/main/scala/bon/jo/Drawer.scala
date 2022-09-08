package bon.jo.pong

import bon.jo.pong.Board
import bon.jo.pong.PongSystem
import bon.jo.pong.Ball
import bon.jo.Geom2D.Segment
import bon.jo.Geom2D.ComputedPath

trait Drawer[C] {
    type CanvasDraw[A] = C ?=> A
    def clear(b : Board):CanvasDraw[Unit]
    inline def ctx : CanvasDraw[C] = summon
    extension (p : Rock)
      def draw() :CanvasDraw[Unit]
    extension (p : Gift)
      def draw() :CanvasDraw[Unit] = (p : Shape).draw()
    extension (p : PongSystem)
      def draw() :CanvasDraw[Unit] = 

        clear(p.board)
        p.board.draw()
        p.balls.foreach(_.draw())
        p.player.foreach(_.draw())
        p.rocks.foreach(_.draw())
        p.gifts.foreach(_.draw())
    extension (board : Board)
      def draw() :CanvasDraw[Unit] = 
        
        board.paths.foreach{
          p => 
            p.draw()
        }
    extension (cp : ComputedPath)
      def draw() :CanvasDraw[Unit] = 
        cp.segments.foreach{ s =>
          s.draw()
        }
    extension (cp : Shape)
      def draw() :CanvasDraw[Unit] = 
        cp.valuep.segments.foreach{ s =>
          s.draw()
        }
    extension (ball : Ball)
      def draw() :CanvasDraw[Unit] 
    extension (ball : Segment)
      def draw() :CanvasDraw[Unit] 
    extension (player : Player)
      def draw() :CanvasDraw[Unit] 
}
