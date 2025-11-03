package casa

trait Monoid[A] extends Semigroup[A]:
  def empty: A

object Monoid:
  object Instances:
    given intAdditionMonoid: Monoid[Int] = new Monoid[Int]:
      override def empty: Int = 0
      override def combine(a1: Int, a2: Int): Int = a1 + a2
    given listConcatenationMonoid[A]: Monoid[List[A]] = new Monoid[List[A]]:
      override def empty: List[A] = List.empty
      override def combine(a1: List[A], a2: List[A]): List[A] = a1 ::: a2
