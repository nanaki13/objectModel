import bon.jo.server.JsonSupport


object TestToken {


  def launchTest(): Unit = {}


}

//@main
def test() = 
  TestToken.launchTest()

trait Monad[F[_]]:
  def pure[A](a : A) : F[A]
  def map[A,B](me : F[A])(f : A=> B) : F[B] = flatMap(me)(e => pure(f(e)))
  def flatMap[A,B](me : F[A])(f : A=> F[B]) : F[B]
  def flatten[A](me : F[F[A]]) : F[A]


extension [A,F[_]](me : F[A])(using z:  Monad[F])
  def map[B](f : A=> B) :  F[B] = z.map[A,B](me)(f)
  def flatMap[B](f : A=> F[B]) :  F[B] = z.flatMap[A,B](me)(f)
  def flatten[B](using f : A => F[B]) :  F[B] = z.flatten[B](me.map(f))

  
