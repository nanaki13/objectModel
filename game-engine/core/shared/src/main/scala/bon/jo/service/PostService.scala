package bon.jo.service
import bon.jo.domain.PostUser
import bon.jo.common.typeutils.~
import scala.concurrent.Future
import bon.jo.domain.PostInfo
import bon.jo.domain.Post
trait PostService:
    def readPosts(subjectId : Int,from : Int,size : Int):Future[Seq[PostUser]]
    def addPost(subjectId : Int,content : PostInfo):Future[Post]
object PostService:
    inline def postService: ~[PostService] = summon
