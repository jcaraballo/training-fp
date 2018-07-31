package casa

import scala.language.higherKinds

// Applicative functor (`pure` and `map2` as primitives)
trait Applicative[F[_]] {
  def pure[A](a: A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(op: (A, B) => C): F[C]

  def map[A, B](fa: F[A])(op: A => B): F[B] = map2(fa, pure(()))((a, _) => op(a))
  def traverse[A, B](as: List[A])(op: A => F[B]): F[List[B]] =
    as.foldRight(pure(List.empty[B]))((a, acc) => map2(op(a), acc)(_ :: _))
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def apply[A, B](fop: F[A => B])(fa: F[A]): F[B] = map2(fop, fa)((op, a) => op(a))
}

object Applicative {
  object Instances {
    implicit def validatedNelApplicative[I]: Applicative[({type IValidatedNel[V] = ValidatedNel[I, V]})#IValidatedNel] =
      new Applicative[({type IValidatedNel[V] = ValidatedNel[I, V]})#IValidatedNel] {
        override def pure[A](a: A): ValidatedNel[I, A] = Valid(a)

        override def map2[A, B, C](fa: ValidatedNel[I, A], fb: ValidatedNel[I, B])(op: (A, B) => C): ValidatedNel[I, C] = (fa, fb) match {
          case (Valid(a), Valid(b)) =>
            Valid(op(a, b))
          case (Invalid(i), Valid(_)) =>
            Invalid(i)
          case (Valid(_), Invalid(i)) =>
            Invalid(i)
          case (Invalid(i1), Invalid(i2)) =>
            Invalid(i1 ::: i2)
        }
      }
  }
}
