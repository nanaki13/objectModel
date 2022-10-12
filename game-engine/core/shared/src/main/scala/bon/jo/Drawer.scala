package bon.jo.pong

import bon.jo.pong.Board
import bon.jo.pong.PongSystem
import bon.jo.pong.Ball
import bon.jo.Geom2D.Segment
import bon.jo.Geom2D.ComputedPath
import bon.jo.Geom2D.Path
import bon.jo.common.typeutils.~

trait DoDraw[C,P]:
  def apply(p : P): C ?=> Unit
trait Ctx[C]:
  inline def ctx : ~[C] = summon
trait Drawer[C] extends  Ctx[C]{
    type CanvasDraw[A] = C ?=> A
    type _DoDraw[P] = DoDraw[C,P]
    type CustomizedDraw[P,A] = (C,_DoDraw[P] )?=>  A
    def clear(b : Board):CanvasDraw[Unit]
    
    inline def gdraw[P] : _DoDraw[P]?=>_DoDraw[P]= summon
    extension [T] (p : T)
      def draw() :CustomizedDraw[T,Unit] = gdraw(p)
    extension (s: Path)
      def fill(): CanvasDraw[Unit] 
   

    extension (p : PongSystem)(using _DoDraw[Rock],_DoDraw[Player],_DoDraw[Board],_DoDraw[Gift])
      def draw() :CanvasDraw[Unit] = 

        clear(p.board)
        p.board.draw()
        p.balls.foreach(_.draw())
        p.player.foreach(_.draw())
        p.rocks.foreach(_.draw())
        p.gifts.foreach(_.draw())

    extension (cp : ComputedPath)
      def draw() :CanvasDraw[Unit] = 
        cp.segments.foreach{ s =>
          s.draw()
        }
    extension (cp : Shape)
      def drawDef() :CanvasDraw[Unit] = 
        cp.valuep.segments.foreach{ s =>
          s.draw()
        }
    extension (ball : Ball)
      def draw() :CanvasDraw[Unit] 
    extension (ball : Segment)
      def draw() :CanvasDraw[Unit] 

}
