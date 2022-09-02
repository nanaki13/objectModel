package bon.jo

object Log {
  object NoLog:
    given LogConf = LogConf(false)
  object WithLog:
    given LogConf = LogConf(true)
  case class LogConf(log : Boolean)
  type Log[T] = LogConf ?=> T
  inline def isLog : Log[Boolean] = summon.log
  inline def log[T](message : String, t : T):Log[T] = 
    if isLog then println(message+t)
    t
}
