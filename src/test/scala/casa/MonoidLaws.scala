package casa

import org.scalacheck.Arbitrary
import org.scalatest.Matchers.convertToAnyShouldWrapper

trait MonoidLaws extends SemigroupLaws {
  def monoidSatisfiesLaws[A](implicit monoid: Monoid[A], arb: Arbitrary[A]): Unit = {
    semigroupSatisfiesAssociativityLaw[A]
    monoidSatisfiesIdentityLaw[A]
  }

  def monoidSatisfiesIdentityLaw[A](implicit monoid: Monoid[A], arb: Arbitrary[A]): Unit = {
    forAll { a: A =>
      monoid.combine(a, monoid.empty) shouldBe a
      monoid.combine(monoid.empty, a) shouldBe a
    }
  }
}
