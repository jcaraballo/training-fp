package casa

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class SemigroupSpec extends AnyFreeSpec {
  "Semigroup" - {
    "Nel is a semigroup" - {
      import Semigroup.Instances.{nonEmptyListConcatenationSemigroup as nelSemigroup}

      "operations" in {
        nelSemigroup.combine(Nel(1, 2), Nel(3, 4)) shouldBe Nel(1, 2, 3, 4)
      }

      "associativity law" in {
        val nelA = Nel(1, 2)
        val nelB = Nel(3, 4)
        val nelC = Nel(5, 6)
        nelSemigroup.combine(
          nelSemigroup.combine(
            nelA,
            nelB
          ),
          nelC
        ) shouldBe
          nelSemigroup.combine(
            nelA, nelSemigroup.combine(
              nelB, nelC
            )
          )
      }
    }
  }
}
