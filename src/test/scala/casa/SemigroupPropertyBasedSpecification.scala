package casa

import org.scalacheck.{Arbitrary, Prop, Properties}
import org.scalacheck.Prop.forAll
import casa.Semigroup.Instances.nonEmptyListConcatenationSemigroup as nelSemigroup

object SemigroupPropertyBasedSpecification extends Properties("Semigroup") {
  given [A](using Arbitrary[A], Arbitrary[List[A]]): Arbitrary[Nel[A]] = {
    Arbitrary(
      for {
        head <- Arbitrary.arbitrary[A]
        tail <- Arbitrary.arbitrary[List[A]]
      } yield Nel(head, tail)
    )
  }

  property("There is a semigroup for Nel where combine is the concatenation of the Nels") =
    forAll { (intNel1: Nel[Int], intNel2: Nel[Int]) =>
      nelSemigroup.combine(intNel1, intNel2) == Nel(intNel1.head, intNel1.tail ++ (intNel2.head :: intNel2.tail))
    }

  property("A Nel is a Semigroup: satisfies associativity law") = {
    semigroupAssociativityLaw[Nel[Int]] &&
      semigroupAssociativityLaw[Nel[String]] &&
      semigroupAssociativityLaw[Nel[Unit]]
  }

  def semigroupAssociativityLaw[A](using semigroup: Semigroup[A], arb: Arbitrary[A]): Prop =
    forAll { (a1: A, a2: A, a3: A) =>
      semigroup.combine(semigroup.combine(a1, a2), a3) == semigroup.combine(a1, semigroup.combine(a2, a3))
    }
}
