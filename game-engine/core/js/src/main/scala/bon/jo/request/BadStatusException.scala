package bon.jo.request

case class BadStatusException[KO](value : KO) extends RuntimeException
