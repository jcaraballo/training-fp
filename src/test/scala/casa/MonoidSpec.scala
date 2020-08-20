package casa

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class MonoidSpec extends AnyFreeSpec {
  "Monoid" - {
    "Int addition is a monoid" - {
      import Monoid.Instances.intAdditionMonoid

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
  }
}
