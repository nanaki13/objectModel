package bon.jo
package domain:

  case class ScoreInfo(idGame: Int, lvl: Int, scoreValue: Long)
  case class GameLevel(idGame: Int, lvl: Int)
  case class GameLevelUser(idGame: Int, lvl: Int, idUser: Long)
  case class Score(
      idGame: Int,
      lvl: Int,
      idUser: Long,
      scoreDateTime: String,
      scoreValue: Long
  )
  case class UserScore(user: UserInfo, score: Score)
  case class Post(idSubject: Int,idUser: Long, postDateTime: String, content: String)
  case class PostInfo(content: String)
  case class PostUser(idSubject: Int,user: UserInfo, postDateTime: String, content: String)
  case class PostSubjectTitle(id: Int, title: String)
  case class PostSubject(postSubjectTitle: PostSubjectTitle, posts: Seq[PostUser])
  package fake:
    def PostSubject(n : Int,title : String, date : ()=> String):PostSubject = 
      domain.PostSubject(PostSubjectTitle(0,title),{
        for{
          i <- 0 until n
          post = PostUser(0,UserInfo(i,"user"+i),date(),("contesnt"+i+" ") * 35)
        } yield post
      })
