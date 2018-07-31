package casa

import org.scalacheck.Arbitrary
import org.scalatest.Matchers.convertToAnyShouldWrapper
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.language.higherKinds

trait MonadLaws extends GeneratorDrivenPropertyChecks {
  def monadSatisfiesLeftIdentityLaw[F[_], A, B](implicit monad: Monad[F], arbA: Arbitrary[A], arbAtoFB: Arbitrary[A => F[B]]): Unit =
    forAll { (a: A, op: A => F[B]) =>
      monad.flatMap(monad.pure(a))(op) shouldBe op(a)
    }

  def monadSatisfiesRightIdentityLaw[F[_], A](implicit monad: Monad[F], arbFA: Arbitrary[F[A]]): Unit =
    forAll { fa: F[A] =>
      monad.flatMap(fa)(monad.pure) shouldBe fa
    }

  def monadSatisfiesAssociativityLaw[F[_], A, B, C](
                                                     implicit monad: Monad[F],
                                                     arbFA: Arbitrary[F[A]],
                                                     arbAtoFB: Arbitrary[A => F[B]],
                                                     arbBtoFC: Arbitrary[B => F[C]]
                                                   ): Unit =
    forAll { (fa: F[A], op1: A => F[B], op2: B => F[C]) =>
      monad.flatMap(monad.flatMap(fa)(op1))(op2) shouldBe monad.flatMap(fa)(a => monad.flatMap(op1(a))(op2))
    }

  def monadSatisfiesLeftIdentityLawWithKleisliComposition[F[_], A, B](implicit monad: Monad[F], arbA: Arbitrary[A], arbAtoFB: Arbitrary[A => F[B]]): Unit =
    forAll { (a: A, op: A => F[B]) =>
      monad.compose(monad.pure[A], op)(a) shouldBe op(a)
    }

  def monadSatisfiesRightIdentityLawWithKleisliComposition[F[_], A, B](implicit monad: Monad[F], arbA: Arbitrary[A], arbAtoFB: Arbitrary[A => F[B]]): Unit =
    forAll { (a: A, op: A => F[B]) =>
      monad.compose(op, monad.pure[B])(a) shouldBe op(a)
    }

  def monadSatisfiesAssociativityLawWithKleisliComposition[F[_], A, B, C, D](
                                                                             implicit monad: Monad[F],
                                                                             arbA: Arbitrary[A],
                                                                             arbAtoFB: Arbitrary[A => F[B]],
                                                                             arbBtoFC: Arbitrary[B => F[C]],
                                                                             arbCtoFB: Arbitrary[C => F[D]]
                                                                           ): Unit =
    forAll { (a: A, op1: A => F[B], op2: B => F[C], op3: C => F[D]) =>
      monad.compose(monad.compose(op1, op2), op3)(a) shouldBe monad.compose(op1, monad.compose(op2, op3))(a)
    }
}
