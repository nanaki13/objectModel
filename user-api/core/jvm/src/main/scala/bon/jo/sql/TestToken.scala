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
def test() = TestToken.launchTest()

  
