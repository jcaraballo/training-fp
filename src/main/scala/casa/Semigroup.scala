package casa

trait Semigroup[A] {
  def combine(a1: A, a2: A): A
}

object Semigroup {
  def apply[A](combineOperation: (A, A) => A): Semigroup[A] = (a1: A, a2: A) => combineOperation(a1, a2)

  object Instances {
    implicit def nonEmptyListConcatenationSemigroup[A]: Semigroup[Nel[A]] = Semigroup(_ ::: _)
  }
}
