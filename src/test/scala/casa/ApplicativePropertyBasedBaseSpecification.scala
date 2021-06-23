package casa

import casa.ApplicativeBaseSpec.{ApplicativeLike, StringValidatedNel}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}


object ApplicativePropertyBasedSpecification extends ApplicativePropertyBasedBaseSpecification(ApplicativeProxy, "Applicative")

object Applicative2PropertyBasedSpecification extends ApplicativePropertyBasedBaseSpecification(Applicative2Proxy, "Applicative2")

abstract class ApplicativePropertyBasedBaseSpecification(va: ApplicativeLike[StringValidatedNel], name: String) extends Properties(name) {

  implicit def arbitraryValidated[A](implicit arbA: Arbitrary[A]): Arbitrary[ValidatedNel[String, A]] = {
    import ValidatedNel.ValidatedNelOps
    Arbitrary(Gen.oneOf(
      arbitrary[String].map(_.invalidNel[A]),
      arbitrary[A].map(_.validNel[String])
    ))
  }

  implicit def arbitraryValid[A](implicit arbA: Arbitrary[A]): Arbitrary[Valid[String, A]] =
    Arbitrary(arbitrary[A].map(Valid.apply))

  property("validated nel: operations: primitives: pure") = {
    validatedNelApplicativeHasPure[String] &&
      validatedNelApplicativeHasPure[Boolean] &&
      validatedNelApplicativeHasPure[Int] &&
      validatedNelApplicativeHasPure[Double]
  }

  property("validated nel: operations: primitives: map2") = {
    validatedNelApplicativeHasMap2[Int, Int, Int] &&
      validatedNelApplicativeHasMap2[String, Int, Boolean] &&
      validatedNelApplicativeHasMap2[Double, Double, Double]
  }

  property("validated nel: operations: derived: map") = {
    validatedNelApplicativeHasMap[Int, Int] &&
      validatedNelApplicativeHasMap[Double, String] &&
      validatedNelApplicativeHasMap[Int, Boolean]
  }

  property("validated nel: operations: derived: traverse") = {
    validatedNelApplicativeHasTraverse[String, Int] &&
      validatedNelApplicativeHasTraverse[Double, Int] &&
      validatedNelApplicativeHasTraverse[Boolean, Boolean]
  }


  protected def validatedNelApplicativeHasPure[A](implicit arbA: Arbitrary[A]): Prop =
    forAll { (a: A) =>
      import ValidatedNel.ValidatedNelOps
      va.pure(a) == a.validNel
    }

  protected def validatedNelApplicativeHasMap2[A, B, C](implicit
                                                        arbA: Arbitrary[A],
                                                        arbB: Arbitrary[B],
                                                        arbC: Arbitrary[C],
                                                        arbAandBtoC: Arbitrary[(A, B) => C]
                                                       ): Prop = {
    import ValidatedNel.ValidatedNelOps

    def map2AppliesOperationToElementsWhenTheyAreBothValid: Prop =
      forAll { (a: A, b: B, op: (A, B) => C) =>
        va.map2(a.validNel, b.validNel)(op) == op(a, b).validNel
      }

    def map2ReturnsFirstElementWhenInvalidButSecondIsValid: Prop =
      forAll { (err: String, b: B, op: (A, B) => C) =>
        va.map2(err.invalidNel[A], b.validNel[String])(op) == err.invalidNel
      }

    def map2ReturnsSecondElementWhenInvalidButFirstIsValid: Prop =
      forAll { (a: A, err: String, op: (A, B) => C) =>
        va.map2(a.validNel[String], err.invalidNel)(op) == err.invalidNel
      }

    def map2ReturnsInvalidCombiningBothErrorsWhenBothAreInvalid: Prop =
      forAll { (err1: String, err2: String, op: (A, B) => C) =>
        // Note how we get both errors
        va.map2(err1.invalidNel, err2.invalidNel)(op) == Invalid(err1 :: Nel(err2))
      }

    map2AppliesOperationToElementsWhenTheyAreBothValid &&
      map2ReturnsFirstElementWhenInvalidButSecondIsValid &&
      map2ReturnsSecondElementWhenInvalidButFirstIsValid &&
      map2ReturnsInvalidCombiningBothErrorsWhenBothAreInvalid
  }

  protected def validatedNelApplicativeHasMap[A, B](implicit
                                                    arbA: Arbitrary[A],
                                                    arbAtoB: Arbitrary[A => B]
                                                   ): Prop = {
    import ValidatedNel.ValidatedNelOps

    def mapAppliesOperationWhenValid = forAll { (a: A, op: A => B) =>
      va.map(a.validNel)(op) == op(a).validNel
    }

    def mapReturnsInputWhenInvalid =
      forAll { (err: String, op: A => B) =>
        va.map(err.invalidNel[A])(op) == err.invalidNel
      }

    mapAppliesOperationWhenValid && mapReturnsInputWhenInvalid
  }

  protected def validatedNelApplicativeHasTraverse[A, B](implicit
                                                         arbListOfA: Arbitrary[List[A]],
                                                         arbAtoB: Arbitrary[A => B],
                                                         arbAtoValidatedNelStringB: Arbitrary[A => ValidatedNel[String, B]],
                                                         arbAtoValidStringB: Arbitrary[A => Valid[String, B]]
                                                        ): Prop = {
    import ValidatedNel.ValidatedNelOps

    def traverseOfEmptyIsAValidEmpty: Prop =
      forAll { (aToValidatedNelStringB: A => ValidatedNel[String, B]) =>
        va.traverse(List.empty[A])(aToValidatedNelStringB) == List.empty[B].validNel[String]
      }

    def traverseReturnsValidOfAllTheContentsOfTheResultsWhenAllTheResultsAreValid: Prop =
      forAll { (as: List[A], aToValidStringB: A => Valid[String, B]) =>
        va.traverse(as)(aToValidStringB) == as.map(aToValidStringB).map(_.v).validNel[String]
      }

    def traverseReturnsInvalidOfAllTheContentsOfTheInvalidResultsWhenAnyOfThemAreInvalid: Prop =
      forAll { (as: List[A], aToValidatedNelStringB: A => ValidatedNel[String, B]) =>
        val bs = as.map(aToValidatedNelStringB)
        bs.exists(_.isInstanceOf[Invalid[String, B]]) ==> (va.traverse(as)(aToValidatedNelStringB) == Invalid[String, List[B]](bs.collect { case Invalid(nel) => nel }.reduce[Nel[String]](_ ::: _)))
      }

    traverseOfEmptyIsAValidEmpty && traverseReturnsValidOfAllTheContentsOfTheResultsWhenAllTheResultsAreValid && traverseReturnsInvalidOfAllTheContentsOfTheInvalidResultsWhenAnyOfThemAreInvalid
  }
}
