package casa

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(op: A => B): F[B]
}

object Functor {
  object Instances {
    implicit val listFunct: Functor[List] = new Functor[List] {
      override def map[A, B](fa: List[A])(op: A => B): List[B] = fa map op
    }
  }
}
