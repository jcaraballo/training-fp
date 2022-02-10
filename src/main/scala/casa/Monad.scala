package casa

trait Monad[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(op: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(op: A => B): F[B] = flatMap(fa)(a => pure(op(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(op: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => op(a, b)))

  def traverse[A, B](as: List[A])(op: A => F[B]):F[List[B]] =
    as.foldRight(pure(List.empty[B]))((a, acc) => map2(op(a), acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse[F[A], A](fas)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  // Kleisli composition
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)


object Monad:
  object Instances:
    given listMonad: Monad[List] = new Monad[List]:
      override def pure[A](a: A): List[A] = List(a)
      override def flatMap[A, B](fa: List[A])(op: A => List[B]): List[B] = fa flatMap op

    given optionMonad: Monad[Option] = new Monad[Option]:
      override def pure[A](a: A): Option[A] = Some(a)
      override def flatMap[A, B](fa: Option[A])(op: A => Option[B]): Option[B] = fa flatMap op

    given eitherMonad[L]: Monad[[B] =>> Either[L, B]] =
      new Monad[[B] =>> Either[L, B]]:
        override def pure[R](a: R): Either[L, R] = Right(a)
        override def flatMap[R1, R2](fa: Either[L, R1])(op: R1 => Either[L, R2]): Either[L, R2] = fa flatMap op

    given stateMonad[S]: Monad[[A] =>> State[S, A]] = new Monad[[A] =>> State[S, A]]:
      override def pure[A](a: A): State[S, A] = State.pure[S, A](a)
      override def flatMap[A, B](fa: State[S, A])(op: A => State[S, B]): State[S, B] = fa flatMap op
