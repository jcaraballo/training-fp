package casa

import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.Matchers.convertToAnyShouldWrapper

trait SemigroupLaws extends GeneratorDrivenPropertyChecks {
  def semigroupSatisfiesAssociativityLaw[A](implicit semigroup: Semigroup[A], arb: Arbitrary[A]): Unit =
    forAll { (a1: A, a2: A, a3: A) =>
      semigroup.combine(semigroup.combine(a1, a2), a3) shouldBe semigroup.combine(a1, semigroup.combine(a2, a3))
    }
}
