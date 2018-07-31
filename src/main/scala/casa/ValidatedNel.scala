package casa

// Simplified implementation
sealed trait ValidatedNel[I, V]
object ValidatedNel {
  implicit class ValidatedNelOps[A](a: A){
    def validNel[I]: ValidatedNel[I, A] = Valid(a)
    def invalidNel[V]: ValidatedNel[A, V] = Invalid(Nel(a))
  }
}
case class Valid[I, V](v: V) extends ValidatedNel[I, V]
case class Invalid[I, V](li: Nel[I]) extends ValidatedNel[I, V]

case class Nel[A](head: A, tail: List[A] = Nil) {
  def ::(a: A): Nel[A] = Nel(a, head :: tail)
  def :::(other: Nel[A]): Nel[A] = Nel(other.head, other.tail ::: (head :: tail))
}
