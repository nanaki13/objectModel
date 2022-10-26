package bon.jo.service

import akka.actor.typed.{ ActorRef, Behavior }
import akka.actor.typed.scaladsl.Behaviors

import PostRepo.Command.*
import PostRepo.Response.*
import bon.jo.service.SqlServicePost.ServicePost

import bon.jo.model.PostModel.{Post,PostUser}

object PostRepo {


  // Trait defining successful and failure responses
  enum Response:
    case OK
    case KO(reason: String)

  // Trait and its implementations representing all possible messages that can be sent to this Behavior
  enum Command:
    case  AddPost(post: Post, replyTo: ActorRef[Response]) extends Command
    case  ReadPosts(subjectId: Int,from: Long,size : Int, replyTo: ActorRef[Seq[PostUser]]) extends Command


  // This behavior handles all possible incoming messages and keeps the state in the function parameter
  def apply(posts: ServicePost): Behavior[Command] = Behaviors.receiveMessage {
    m => 
      m match
        case AddPost(post, replyTo) =>
          posts.create(post)
           replyTo ! Response.OK
          
        case ReadPosts(subjectId, from, size, replyTo) =>

          val post = posts.readPost(subjectId,from,size)
          replyTo ! post

        Behaviors.same
  }
}