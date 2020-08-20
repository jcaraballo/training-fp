package casa

import org.scalatest.FreeSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class SemigroupSpec extends FreeSpec {
  "Semigroup" - {
    "Nel is a semigroup" - {
      import Semigroup.Instances.{nonEmptyListConcatenationSemigroup => nelSemigroup}

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
