package bon.jo
package domain : 
  case class ScoreInfo(idGame : Int,lvl : Int,scoreValue : Long)
  
  case class GameLevel(idGame : Int,lvl : Int)
  case class GameLevelUser(idGame : Int,lvl : Int,idUser : Long)



