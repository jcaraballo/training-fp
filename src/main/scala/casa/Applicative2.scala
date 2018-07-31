package casa

import scala.language.higherKinds

// An applicative functor defined using `pure` and `apply` as primitives
trait Applicative2[F[_]] {
  def pure[A](a: A): F[A]
  def apply[A, B](fop: F[A => B])(fa: F[A]): F[B]

  def map[A, B](fa: F[A])(op: A => B): F[B] = apply(pure(op))(fa)
  def map2[A, B, C](fa: F[A], fb: F[B])(op: (A, B) => C): F[C] = apply(apply(pure(op.curried))(fa))(fb)
  def traverse[A, B](as: List[A])(op: A => F[B]): F[List[B]] =
    as.foldRight(pure(List.empty[B]))((a, acc) => map2(op(a), acc)(_ :: _))
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
}

object Applicative2 {
  object Instances {
    implicit def validatedNelApplicative2[I]: Applicative2[({type IValidatedNel[V] = ValidatedNel[I, V]})#IValidatedNel] = new Applicative2[({type IValidatedNel[V] = ValidatedNel[I, V]})#IValidatedNel] {
      override def pure[A](a: A): ValidatedNel[I, A] = Valid(a)

      override def apply[A, B](fop: ValidatedNel[I, A => B])(fa: ValidatedNel[I, A]): ValidatedNel[I, B] = (fop, fa) match {
        case (Valid(op), Valid(a)) => Valid(op(a))
        case (Invalid(i), Valid(_)) => Invalid(i)
        case (Valid(_), Invalid(i)) => Invalid(i)
        case (Invalid(i1), Invalid(i2)) => Invalid(i1 ::: i2)
      }
    }
  }
}
