import cats.Applicative
import cats.implicits.catsSyntaxTuple2Semigroupal

object Traverse{
  def traverse[F[_] : Applicative, A, B](listA: List[A])(f: A => F[B]): F[List[B]] = {
    listA match {
      case Nil => Applicative[F].pure(Nil)
      case head :: tail =>
        (f(head), traverse(tail)(f)).mapN{(h, t) => h :: t}
    }
  }

}
