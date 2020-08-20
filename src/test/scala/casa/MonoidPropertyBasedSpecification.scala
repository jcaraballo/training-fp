package casa

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop, Properties}

object MonoidPropertyBasedSpecification extends Properties("Monoid") {
  implicit def arbitraryNel[A](implicit arbA: Arbitrary[A], arbListA: Arbitrary[List[A]]): Arbitrary[Nel[A]] = {
    Arbitrary(
      for {
        head <- Arbitrary.arbitrary[A]
        tail <- Arbitrary.arbitrary[List[A]]
      } yield Nel(head, tail)
    )
  }

  property("There is a monoid for Int where combine is addition and empty is 0") =
    forAll { (int1: Int, int2: Int) =>
      Monoid.Instances.intAdditionMonoid.combine(int1, int2) == int1 + int2
    }

  property("The addition monoid for Int satisfies monoid laws") =
    monoidLaws[Int](Monoid.Instances.intAdditionMonoid, implicitly[Arbitrary[Int]])

  def monoidLaws[A](implicit monoid: Monoid[A], arb: Arbitrary[A]): Prop = {
    SemigroupPropertyBasedSpecification.semigroupAssociativityLaw[A] &&
    monoidIdentityLaw[A]
  }

  def monoidIdentityLaw[A](implicit monoid: Monoid[A], arb: Arbitrary[A]): Prop = {
    forAll { a: A =>
      monoid.combine(a, monoid.empty) == a &&
      monoid.combine(monoid.empty, a) == a
    }
  }
}
