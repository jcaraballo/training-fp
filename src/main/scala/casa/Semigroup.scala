package casa

trait Semigroup[A]:
  def combine(a1: A, a2: A): A

object Semigroup:
  object Instances:
    given nonEmptyListConcatenationSemigroup[A]: Semigroup[Nel[A]] = _ ::: _
