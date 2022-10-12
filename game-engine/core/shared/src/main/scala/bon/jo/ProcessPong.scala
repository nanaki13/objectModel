package bon.jo.pong


import bon.jo.Geom2D.*
import bon.jo.Geom2D.Vector.`*`
import bon.jo.System
import bon.jo.System.*
trait Debug:
   def debug():Unit
   def debug(toDebug  :String):Unit = 
      println(toDebug)
      debug()
object Debug : 
   inline def apply(s  :String):Debug ?=>Unit = summon.debug(s)
   inline def apply():Debug ?=>Unit = summon.debug()


class ProcessPong[C](using Debug,Drawer[C],C) extends SystemProcess[PongSystem]:

   type PongSys[A] = Sys[PongSystem,(Ball,Set[Rock])]
   
 

   def next(): SystemFlow[PongSystem] = 
      var sys = System()
      val balls = sys.balls
      val board : Board = sys.board
      var nPlayer = sys.player.map(_.move[Player]()).map(_.applyEffect())
      sys = sys.copy(player = nPlayer)
      val ballsRocks =  balls.map(process(sys))
      val ballsMod = ballsRocks.map(_._1)
      val goodBall  = ballsMod.filter{
         b => b.pos.x < board.maxX +10 && b.pos.x > board.minX - 10 && 
            b.pos.y < board.maxY +10 && b.pos.y > board.minY - 10d
      }
      val rocksToRem : Seq[Rock] = ballsRocks.flatMap(_._2)
      
      val giftsToAdd : Seq[Gift] = rocksToRem.flatMap(e => e.gift)
      val agg : (Seq[(Gift,Player)],Seq[Gift])= (Nil,Nil)
      
      val (giftReachByPlayer,other) = (for{
         gift <- sys.gifts
         player <- nPlayer
         cross = if gift.valuep.boundaryCross(player.path) then Some(gift,player) else None

      } yield (gift,player)).foldLeft(agg)((agg,el)=>{
         if el._1.valuep.boundaryCross(el._2.path) then
            (agg._1:+el,agg._2) 
         else
           (agg._1,agg._2:+el._1) 
      })
      
      val giftsUpdate : Seq[Gift] = other.map(_.move[Gift]())
      val points = rocksToRem.size * 50 + giftReachByPlayer.size * 75
      sys = sys.copy(balls = goodBall.map(_.applyEffect()),player = nPlayer.map(_.addToScore(points)),rocks = sys.rocks.filter(e => !rocksToRem.contains(e)),gifts = giftsUpdate ++ giftsToAdd )
      giftReachByPlayer.foldLeft(sys)(resolveGift(_).tupled(_))

   def resolveGift(sys: PongSystem)(gift : Gift,player : Player):PongSystem = 
      gift match
         case _ : Gift.NewBall => 
            val headBall = sys.balls.head
            val nBall = headBall.withPosAndSpeed(headBall.pos,-headBall.speed)
            sys.copy(balls = sys.balls :+ nBall)
         case _ : Gift.GreaterPlayer => sys.copy(player = sys.player.map(e => e.copy(effects = e.effects :+ Player.multSizeEffect(10,1.015))))
         case _ : Gift.GreaterBall => sys.copy(balls = sys.balls.map(e => e.copy(effects = e.effects :+ Ball.multSizeEffect(10,1.015))))
         case _ : Gift.SmallerBall => sys.copy(balls = sys.balls.map(e => e.copy(effects = e.effects :+ Ball.multSizeEffect(10,0.99))))
         case _ : Gift.SmallerPlayer => sys.copy(player = sys.player.map(e => e.copy(effects = e.effects :+ Player.multSizeEffect(10,0.99))))
         case _ : Gift.FasterPlayer => sys.copy(player = sys.player.map(e => e.copy(effects = e.effects :+ Player.multSpeedEffect(10,1.01))))
         case _ : Gift.FasterBall => sys.copy(balls = sys.balls.map(e => e.copy(effects = e.effects :+ PosSpeed.multSpeedEffect(10,1.01))))
         case _ : Gift.SlowBall => sys.copy(balls = sys.balls.map(e => e.copy(effects = e.effects :+ PosSpeed.multSpeedEffect(10,0.99))))
         case _ : Gift.SlowPlayer => sys.copy(player = sys.player.map(e => e.copy(effects = e.effects :+ Player.multSpeedEffect(10,0.99))))

      

   def process( sys : PongSystem)( b : Ball):(Ball,Set[Rock]) =
      val speedN = b.speed.length
      var dist = 0d
      var tmpE = b
      var crosCount = 0
      
      val board : Board = sys.board
      var rSet  :Set[Rock] = Set.empty
      val segs :List[(SystemElement,Segment)]= board.paths.flatMap(e =>  e.segments.map(board -> _)) ++ sys.player.flatMap(p => p.path.segments.map( p -> _)) ++ 
         sys.rocks.flatMap(r => r.value.segments.map(r -> _))
      val poinImageSources = 
         (for
            p <- b.shape.points()
            (el,seg) <- segs
            ptoNew = p -> b.speed
            cross <- ptoNew.cross(seg)
            newSym = ptoNew.p2.sym(seg)   
         yield
            (Segment(cross,newSym),el,p)).headOption
      for
         el <- poinImageSources.map(_._2)
      yield
         rSet = el match
               case r : Rock => rSet + r
               case o => rSet 
      
      if poinImageSources.nonEmpty then
         val somme = poinImageSources.map(_._1.toVector()).head//.head//.reduce(_ + _)
         
         val nv = 
            if somme != Point(0,0) then
               speedN * somme.unitary()
            else

               b.speed
         (b.withPosAndSpeed(b.pos + nv,nv),rSet)
      else
         (b.withPosAndSpeed( b.pos + b.speed, b.speed),rSet)  
      

            
     
