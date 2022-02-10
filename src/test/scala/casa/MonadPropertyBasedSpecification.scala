package casa

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop, Properties}

object MonadPropertyBasedSpecification extends Properties("Monad") {

  property("List is a Monad, so it satisfies the monad laws") = {
    import Monad.Instances.listMonad

    monadLaws[List, Int, Int, Int] &&
      monadLaws[List, Int, String, Boolean] &&
      monadLaws[List, Double, String, Int] &&
      monadLaws[List, Double, Double, Double]
  }

  property("List is a Monad, so it satisfies the monad laws expressed with Kleisli composition") = {
    import Monad.Instances.listMonad

    monadLawsWithKleisliComposition[List, Int, Int, Int, Int] &&
      monadLawsWithKleisliComposition[List, Int, String, Boolean, Double] &&
      monadLawsWithKleisliComposition[List, Int, Double, String, Boolean] &&
      monadLawsWithKleisliComposition[List, Double, Double, Double, Double]
  }

  property("Option is a Monad") = {
    import Monad.Instances.optionMonad

    monadLaws[Option, Int, Double, String] &&
      monadLawsWithKleisliComposition[Option, Int, Double, String, Boolean]
  }

  property("Either is a Monad") = {
    import Monad.Instances.eitherMonad

    type StringEither[B] = Either[String, B]

    monadLaws[StringEither, Int, Double, String] &&
      monadLawsWithKleisliComposition[StringEither, Int, Double, String, Boolean]
  }

  // Can't really verify the laws as they stand because State instances can't be compared properly (encapsulates a function)
  /*
  property("State is a Monad") = {
    type IntState[A] = State[Int, A]
    implicit def arbState[S, A](implicit sToAS: Arbitrary[S => (A, S)]): Arbitrary[State[S, A]] =
      Arbitrary(arbitrary[S => (A, S)].map(State.apply))

    monadFunctorLaws[IntState, Int, Double, String] &&
    monadMonadLaws[IntState, Int, Double, String] &&
    monadMonadLawsWithKleisliComposition[IntState, Int, Double, String, Boolean]
  }
  */


  def monadLaws[F[_], A, B, C](using Monad[F],
                                     Arbitrary[A],
                                     Arbitrary[F[A]],
                                     Arbitrary[A => B],
                                     Arbitrary[B => C],
                                     Arbitrary[A => F[B]],
                                     Arbitrary[B => F[C]]
                              ): Prop = {
    FunctorPropertyBasedSpecification.functorLaws[F, A, B, C] &&
      monadLeftIdentityLaw[F, A, B] &&
      monadRightIdentityLaw[F, A] &&
      monadAssociativityLaw[F, A, B, C]
  }

  def monadLawsWithKleisliComposition[F[_], A, B, C, D](using Monad[F],
                                                              Arbitrary[A],
                                                              Arbitrary[F[A]],
                                                              Arbitrary[A => B],
                                                              Arbitrary[B => C],
                                                              Arbitrary[A => F[B]],
                                                              Arbitrary[B => F[C]],
                                                              Arbitrary[C => F[D]]
                                                       ): Prop = {
    monadLeftIdentityLawWithKleisliComposition[F, A, B] &&
      monadRightIdentityLawWithKleisliComposition[F, A, B] &&
      monadAssociativityLawWithKleisliComposition[F, A, B, C, D]
  }

  private def monadLeftIdentityLaw[F[_], A, B](using monad: Monad[F], arbA: Arbitrary[A], arbAtoFB: Arbitrary[A => F[B]]): Prop =
    forAll { (a: A, op: A => F[B]) =>
      monad.flatMap(monad.pure(a))(op) == op(a)
    }

  private def monadRightIdentityLaw[F[_], A](using monad: Monad[F], arbFA: Arbitrary[F[A]]): Prop =
    forAll { (fa: F[A]) =>
      monad.flatMap(fa)(monad.pure) == fa
    }

  private def monadAssociativityLaw[F[_], A, B, C](using monad: Monad[F],
                                                         arbFA: Arbitrary[F[A]],
                                                         arbAtoFB: Arbitrary[A => F[B]],
                                                         arbBtoFC: Arbitrary[B => F[C]]
                                                  ): Prop =
    forAll { (fa: F[A], op1: A => F[B], op2: B => F[C]) =>
      monad.flatMap(monad.flatMap(fa)(op1))(op2) == monad.flatMap(fa)(a => monad.flatMap(op1(a))(op2))
    }

  def monadLeftIdentityLawWithKleisliComposition[F[_], A, B](using monad: Monad[F],
                                                                   arbA: Arbitrary[A],
                                                                   arbAtoFB: Arbitrary[A => F[B]]
                                                            ): Prop =
    forAll { (a: A, op: A => F[B]) =>
      monad.compose(monad.pure[A], op)(a) == op(a)
    }

  def monadRightIdentityLawWithKleisliComposition[F[_], A, B](using monad: Monad[F],
                                                                    arbA: Arbitrary[A],
                                                                    arbAtoFB: Arbitrary[A => F[B]]
                                                             ): Prop =
    forAll { (a: A, op: A => F[B]) =>
      monad.compose(op, monad.pure[B])(a) == op(a)
    }

  def monadAssociativityLawWithKleisliComposition[F[_], A, B, C, D](using monad: Monad[F],
                                                                          arbA: Arbitrary[A],
                                                                          arbAtoFB: Arbitrary[A => F[B]],
                                                                          arbBtoFC: Arbitrary[B => F[C]],
                                                                          arbCtoFB: Arbitrary[C => F[D]]
                                                                   ): Prop =
    forAll { (a: A, op1: A => F[B], op2: B => F[C], op3: C => F[D]) =>
      monad.compose(monad.compose(op1, op2), op3)(a) == monad.compose(op1, monad.compose(op2, op3))(a)
    }
}
