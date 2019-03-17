package casa

import org.scalacheck.Arbitrary
import org.scalatest.FreeSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class MonoidSpec extends FreeSpec with MonoidLaws {
  "Monoid" - {
    "Int addition is a monoid" - {
      import Monoid.Instances.intAdditionMonoid

      "example based" - {
        "operations" in {
          intAdditionMonoid.empty shouldBe 0
          intAdditionMonoid.combine(1, 2) shouldBe 3
        }

        "laws" - {
          "combine is associative" in {
            intAdditionMonoid.combine(
              intAdditionMonoid.combine(
                1,
                2
              ),
              3
            ) shouldBe
              intAdditionMonoid.combine(
                1,
                intAdditionMonoid.combine(
                  2,
                  3
                )
              )
          }

          "empty is identity" in {
            intAdditionMonoid.combine(5, intAdditionMonoid.empty) shouldBe 5
            intAdditionMonoid.combine(intAdditionMonoid.empty, 5) shouldBe 5
          }
        }
      }

      "property-based" - {
        "operations" - {
          "combine" in {
            forAll { (int1: Int, int2: Int) =>
              intAdditionMonoid.combine(int1, int2) shouldBe int1 + int2
            }
          }
        }

        "laws" in {
          monoidSatisfiesLaws[Int](intAdditionMonoid, implicitly[Arbitrary[Int]])
        }
      }
    }
  }
}
