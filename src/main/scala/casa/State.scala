package casa

case class State[S, A](run: S => (A, S)):
  def map[B](op: A => B): State[S, B] = State { s =>
    val (a, newS) = run(s)
    (op(a), newS)
  }
  def flatMap[B](op: A => State[S, B]): State[S, B] = State { s =>
    val (a, newS) = run(s)
    op(a).run(newS)
  }

object State:
  def pure[S, A](a: A): State[S, A] = State(s => (a, s))
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
