import pdi.jwt.{JwtJson4s, JwtAlgorithm}, org.json4s._, org.json4s.JsonDSL.WithBigDecimal._
import java.time.Clock
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneOffset
import pdi.jwt.JwtClaim
import pdi.jwt.Jwt
import java.time.Instant
import scala.util.Try
import java.time.OffsetDateTime
import java.time.ZoneId
import java.time.ZonedDateTime
object TestToken {
  given Clock = Clock.systemDefaultZone()

 
  def launchTest(): Unit =
    val claim = JObject(("pseudo", "nanaki"), ("nbf", -1), ("exp", ZonedDateTime.now().minusDays(5).toEpochSecond()))
    // claim: JObject = JObject(
    //   obj = List(("user", JInt(num = 1)), ("nbf", JInt(num = 1431520421)))
    // )
    val key = "secret"
    // key: String = "secretKey"
    val algo = JwtAlgorithm.HS256

    val enc : JwtClaim => String = Jwt.encode(_ , key, algo)
    val dec : String => Try[JwtClaim] = Jwt.decode(_ , key, Seq(algo))
    // algo: JwtAlgorithm.HS256.type = HS256
    
    //println(dec(JwtJson4s.encode(claim,key,algo)).get)
   
    // res0: String = "eyJhbGciOiJub25lIn0.eyJ1c2VyIjoxLCJuYmYiOjE0MzE1MjA0MjF9."

   // val token = JwtJson4s.encode(claim, key, algo)
    val token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjE2NjM3MzgzNjMsImlkIjoxLCJuYW1lIjoibmFuYWtpIn0.BFHFMY622x3IlFBPrT1qBz5O4qiBxNV8TJs4VhovBww"
    println(OffsetDateTime.now(summon))
    println(token)
    // token: String = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyIjoxLCJuYmYiOjE0MzE1MjA0MjF9.VmfmoqRbRvna9lfpCx4lXf96eD_X_woBM0twLjBGLlQ"

    println(JwtJson4s.decodeJson(token, key, Seq(JwtAlgorithm.HS256)))
    println(pdi.jwt.Jwt.decodeRaw(token, key, Seq(JwtAlgorithm.HS256)))
    println(dec(token))
    println(pdi.jwt.Jwt.isValid(token,key,Seq(algo)))
    // res1: util.Try[JObject] = Success(
    //   value = JObject(
    //     obj = List(("user", JInt(num = 1)), ("nbf", JInt(num = 1431520421)))
    //   )
    // )

    val decode = JwtJson4s.decode(token, key, Seq(JwtAlgorithm.HS256))
    decode.foreach{
      dd => 
        println(dd)
        println(dd.isValid)
       
        val d = dd.expiration.get
        println(LocalDateTime.from(Instant.ofEpochSecond(d ).atOffset(ZoneOffset.UTC)))
        println(JwtJson4s.encode(dd,key,algo))
      
    }
    println(JwtClaim(content = """{"pseudo":"nanaki"}""", issuer = Some("1")).toJson)
    // res2: util.Try[pdi.jwt.JwtClaim] = Success(
    //   value = JwtClaim({"user":1}, None, None, None, None, Some(1431520421), None, None)
    // )
}

//@main
def test() = 
  TestToken.launchTest()

  case class Truc[A](a : A)
  given [B] : (Truc[B] => List[B]) = e => List(e.a)
  given [B] : (Truc[B] => List[B]) = e => List(e.a)
  given Monad[Truc] with 

    override def pure[A](a: A): Truc[A] = Truc(a)
    override def flatMap[A, B](me: Truc[A])(f: A => Truc[B]): Truc[B] = f(me.a)

    override def flatten[A](me: Truc[Truc[A]]): Truc[A] = Truc(me.a.a)

  val aa = Truc("1")
  val bb = aa.map(_.toInt)
  val cc = Truc(Truc("1"))
  for{
    i <- aa
    ii <- bb
    iii <- cc.flatten
    ee <- Truc(List(1))
  } yield Truc((i,ii,iii,ee))

  List(Truc(1)).flatten

  


trait Monad[F[_]]:
  def pure[A](a : A) : F[A]
  def map[A,B](me : F[A])(f : A=> B) : F[B] = flatMap(me)(e => pure(f(e)))
  def flatMap[A,B](me : F[A])(f : A=> F[B]) : F[B]
  def flatten[A](me : F[F[A]]) : F[A]


extension [A,F[_]](me : F[A])(using z:  Monad[F])
  def map[B](f : A=> B) :  F[B] = z.map[A,B](me)(f)
  def flatMap[B](f : A=> F[B]) :  F[B] = z.flatMap[A,B](me)(f)
  def flatten[B](using f : A => F[B]) :  F[B] = z.flatten[B](me.map(f))

  
