import pdi.jwt.{JwtJson4s, JwtAlgorithm}, org.json4s._, org.json4s.JsonDSL.WithBigDecimal._
import java.time.Clock
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneOffset
object TestToken :

  implicit val clock: Clock = Clock.systemUTC

  @main
  def m():Unit=
    val claim = JObject(("user", 1), ("nbf", -1), ("exp", LocalDateTime.now().plusDays(5).toEpochSecond(ZoneOffset.UTC)))
    // claim: JObject = JObject(
    //   obj = List(("user", JInt(num = 1)), ("nbf", JInt(num = 1431520421)))
    // )
    val key = "secretKey"
    // key: String = "secretKey"
    val algo = JwtAlgorithm.HS256
    // algo: JwtAlgorithm.HS256.type = HS256

    println(JwtJson4s.encode(claim))
    // res0: String = "eyJhbGciOiJub25lIn0.eyJ1c2VyIjoxLCJuYmYiOjE0MzE1MjA0MjF9."

    val token = JwtJson4s.encode(claim, key, algo)
    println(token)
    // token: String = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1c2VyIjoxLCJuYmYiOjE0MzE1MjA0MjF9.VmfmoqRbRvna9lfpCx4lXf96eD_X_woBM0twLjBGLlQ"

    println(JwtJson4s.decodeJson(token, key, Seq(JwtAlgorithm.HS256)))
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
        println(dd.expiresAt)
      
    }
    // res2: util.Try[pdi.jwt.JwtClaim] = Success(
    //   value = JwtClaim({"user":1}, None, None, None, None, Some(1431520421), None, None)
    // )
