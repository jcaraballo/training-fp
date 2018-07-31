package casa

import org.scalacheck.Arbitrary
import org.scalatest.Matchers.convertToAnyShouldWrapper
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.language.higherKinds

trait FunctorLaws extends GeneratorDrivenPropertyChecks {
  def functorSatisfiesIdentityLaw[F[_], A](implicit functor: Functor[F], arb: Arbitrary[F[A]]): Unit =
    forAll { fa: F[A] =>
      functor.map(fa)(identity) shouldBe fa
    }


  def functorSatisfiesCompositionLaw[F[_], A, B, C](implicit functor: Functor[F],
                                                    arbFA: Arbitrary[F[A]],
                                                    arbAtoB: Arbitrary[A => B],
                                                    arbBtoC: Arbitrary[B => C]
                                                   ): Unit =
    forAll { (fa: F[A], opAtoB: A => B, opBtoC: B => C) =>
      functor.map(functor.map(fa)(opAtoB))(opBtoC) shouldBe functor.map(fa)(opBtoC compose opAtoB)
    }
}
