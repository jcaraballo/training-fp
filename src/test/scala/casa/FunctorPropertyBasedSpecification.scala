package casa

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop, Properties}

object FunctorPropertyBasedSpecification extends Properties("Functor") {

  property("List is a Functor") = {
    import Functor.Instances.listFunct

    functorLaws[List, Int, Int, Int] &&
      functorLaws[List, Int, String, Boolean] &&
      functorLaws[List, Double, String, Int] &&
      functorLaws[List, Double, Double, Double]
  }

  def functorLaws[F[_], A, B, C](using Functor[F],
                                       Arbitrary[F[A]],
                                       Arbitrary[A => B],
                                       Arbitrary[B => C]): Prop = {
    functorIdentityLaw[F, A] &&
      functorCompositionLaw[F, A, B, C]
  }

  private def functorIdentityLaw[F[_], A](using functor: Functor[F], arb: Arbitrary[F[A]]): Prop =
    forAll { (fa: F[A]) =>
      functor.map(fa)(identity) == fa
    }


  private def functorCompositionLaw[F[_], A, B, C](using functor: Functor[F],
                                                         arbFA: Arbitrary[F[A]],
                                                         arbAtoB: Arbitrary[A => B],
                                                         arbBtoC: Arbitrary[B => C]
                                                  ): Prop =
    forAll { (fa: F[A], opAtoB: A => B, opBtoC: B => C) =>
      functor.map(functor.map(fa)(opAtoB))(opBtoC) == functor.map(fa)(opBtoC compose opAtoB)
    }
}
