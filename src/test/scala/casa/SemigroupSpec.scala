package casa

import org.scalacheck.Arbitrary
import org.scalatest.FreeSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class SemigroupSpec extends FreeSpec with SemigroupLaws {
  implicit def arbitraryNel[A](implicit arbA: Arbitrary[A], arbListA: Arbitrary[List[A]]): Arbitrary[Nel[A]] = {
    Arbitrary(
      for {
        head <- Arbitrary.arbitrary[A]
        tail <- Arbitrary.arbitrary[List[A]]
      } yield Nel(head, tail)
    )
  }

  "Semigroup" - {
    "Nel is a semigroup" - {
      import Semigroup.Instances.{nonEmptyListConcatenationSemigroup => nelSemigroup}

      "example based" - {
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

      "property-based" - {
        "operation" in {
          forAll { (intNel1: Nel[Int], intNel2: Nel[Int]) =>
            nelSemigroup.combine(intNel1, intNel2) shouldBe
              Nel(intNel1.head, intNel1.tail ++ (intNel2.head :: intNel2.tail))
          }
        }

        "law" in {
          semigroupSatisfiesAssociativityLaw[Nel[Int]]
        }
      }
    }
  }
}
