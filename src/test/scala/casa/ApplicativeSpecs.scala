package casa

import org.scalatest.FreeSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper
import ApplicativeBaseSpec.{ApplicativeLike, StringValidatedNel}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.language.higherKinds

class ApplicativeSpec extends ApplicativeBaseSpec(ApplicativeProxy)
class Applicative2Spec extends ApplicativeBaseSpec(Applicative2Proxy)

abstract class ApplicativeBaseSpec(va: ApplicativeLike[StringValidatedNel]) extends FreeSpec with GeneratorDrivenPropertyChecks {

  protected def validatedNelApplicativeHasPure[A](implicit arbA: Arbitrary[A]): Unit =
    forAll { a: A =>
      import ValidatedNel.ValidatedNelOps
      va.pure(a) shouldBe a.validNel
    }

  protected def validatedNelApplicativeHasMap2[A, B, C](implicit
                                                        arbA: Arbitrary[A],
                                                        arbB: Arbitrary[B],
                                                        arbC: Arbitrary[C],
                                                        arbAandBtoC: Arbitrary[(A, B) => C]
                                                       ): Unit = {
    import ValidatedNel.ValidatedNelOps

    forAll { (a: A, b: B, op: (A, B) => C) =>
      va.map2(a.validNel, b.validNel)(op) shouldBe op(a, b).validNel
    }

    forAll { (err: String, b: B, op: (A, B) => C) =>
      va.map2(err.invalidNel[A], b.validNel[String])(op) shouldBe err.invalidNel
    }

    forAll { (a: A, err: String, op: (A, B) => C) =>
      va.map2(a.validNel[String], err.invalidNel)(op) shouldBe err.invalidNel
    }

    forAll { (err1: String, err2: String, op: (A, B) => C) =>
      // Note how we get both errors
      va.map2(err1.invalidNel, err2.invalidNel)(op) shouldBe Invalid(err1 :: Nel(err2))
    }
  }

  protected def validatedNelApplicativeHasMap[A, B](implicit
                                                        arbA: Arbitrary[A],
                                                        arbAtoB: Arbitrary[A => B]
                                                       ): Unit = {
    import ValidatedNel.ValidatedNelOps

    forAll { (a: A, op: A => B) =>
      va.map(a.validNel)(op) shouldBe op(a).validNel
    }

    forAll { (err: String, op: A => B) =>
      va.map(err.invalidNel[A])(op) shouldBe err.invalidNel
    }
  }

  implicit def arbitraryValid[A](implicit arbA: Arbitrary[A]): Arbitrary[Valid[String, A]] =
    Arbitrary(arbitrary[A].map(Valid.apply))

  implicit def arbitraryValidated[A](implicit arbA: Arbitrary[A]): Arbitrary[ValidatedNel[String, A]] = {
    import ValidatedNel.ValidatedNelOps
    Arbitrary(Gen.oneOf(
      arbitrary[String].map(_.invalidNel[A]),
      arbitrary[A].map(_.validNel[String])
    ))
  }

  protected def validatedNelApplicativeHasTraverse[A, B](implicit
                                                         arbListOfA: Arbitrary[List[A]],
                                                         arbAtoB: Arbitrary[A => B],
                                                         arbAtoValidatedNelStringB: Arbitrary[A => ValidatedNel[String, B]],
                                                         arbAtoValidStringB: Arbitrary[A => Valid[String, B]]
                                                        ): Unit = {
    import ValidatedNel.ValidatedNelOps

    forAll{ aToValidatedNelStringB: (A => ValidatedNel[String, B]) =>
      va.traverse(List.empty[A])(aToValidatedNelStringB) shouldBe List.empty[B].validNel[String]
    }

    forAll { (as: List[A], aToValidStringB: A => Valid[String, B]) =>
      va.traverse(as)(aToValidStringB) shouldBe as.map(aToValidStringB).map(_.v).validNel[String]
    }

    forAll { (as: List[A], aToValidatedNelStringB: A => ValidatedNel[String, B]) =>
      val bs: List[ValidatedNel[String, B]] = as.map(aToValidatedNelStringB)

      whenever(bs.exists(_.isInstanceOf[Invalid[String, B]])) {
        va.traverse(as)(aToValidatedNelStringB) shouldBe Invalid[String, List[B]](bs.collect { case Invalid(nel) => nel }.reduce[Nel[String]](_ ::: _))
      }
    }
  }

  "Applicative Functor" - {
    "ValidatedNel is an Applicative Functor" - {
      import ValidatedNel.ValidatedNelOps

      "operations" - {
        "primitives" - {
          "pure" in {
            validatedNelApplicativeHasPure[String]
            validatedNelApplicativeHasPure[Boolean]
            validatedNelApplicativeHasPure[Int]
            validatedNelApplicativeHasPure[Double]
          }

          "map2" in {
            validatedNelApplicativeHasMap2[Int, Int, Int]
            validatedNelApplicativeHasMap2[String, Int, Boolean]
            validatedNelApplicativeHasMap2[Double, Double, Double]
          }
        }

        "derived" - {
          "map" in {
            validatedNelApplicativeHasMap[Int, Int]
            validatedNelApplicativeHasMap[Double, String]
            validatedNelApplicativeHasMap[Int, Boolean]
          }

          "traverse" in {
            validatedNelApplicativeHasTraverse[String, Int]
            validatedNelApplicativeHasTraverse[Double, Int]
            validatedNelApplicativeHasTraverse[Boolean, Boolean]
          }

          "sequence" in {
            va.sequence(List(1.validNel[String], 2.validNel[String])) shouldBe List(1, 2).validNel

            va.sequence(List("foo".invalidNel[Int], 2.validNel[String])) shouldBe "foo".invalidNel[Int]
            va.sequence(List(1.validNel[String], "bar".invalidNel[Int])) shouldBe "bar".invalidNel[Int]
            va.sequence(List("foo".invalidNel[Int], "bar".invalidNel[Int])) shouldBe Invalid("foo" :: Nel("bar"))

            va.sequence(List.empty) shouldBe List.empty.validNel
          }

          "replicateM" - {
            "n > 0" in {
              va.replicateM(3, 1.validNel[String]) shouldBe List(1, 1, 1).validNel[String]

              // Note again how replicateM replicates whole context, not only the "element", i.e., applied to ValidatedNel
              // both the valid and the invalid cases. Therefore replicateM(..., Invalid(...)) needs to replicate invalid
              // cases too. That's why we implement it as `sequence(List.fill(n)(fa))`. The alternative, an
              // implementation such as `map(fa)(List.fill(n)(_))` would not fulfill the goal of replicateM because it
              // would not replicate invalid cases. In the example below we would get only `"foo".invalidNel`, which is
              // not what we want.
              va.replicateM(3, "foo".invalidNel[Int]) shouldBe Invalid("foo" :: "foo" :: Nel("foo"))
            }

            "n == 0" in {
              va.replicateM(0, 1.validNel[String]) shouldBe List.empty.validNel
              va.replicateM(0, "foo".validNel[String]) shouldBe List.empty.validNel
            }
          }

          "product" in {
            va.product(1.validNel, "a".validNel) shouldBe (1, "a").validNel

            va.product("foo".invalidNel[Int], "a".validNel[String]) shouldBe "foo".invalidNel
            va.product(1.validNel[String], "bar".invalidNel[String]) shouldBe "bar".invalidNel
            va.product("foo".invalidNel[Int], "bar".invalidNel[String]) shouldBe Invalid("foo" :: Nel("bar"))
          }

          "apply" in {
            val intToString: Int => String = _.toString
            va.apply(intToString.validNel[String])(1.validNel) shouldBe "1".validNel

            va.apply(
              "Function who?".invalidNel[Int => String]
            )(1.validNel) shouldBe "Function who?".invalidNel
            va.apply(intToString.validNel[String])("Not a number".invalidNel) shouldBe "Not a number".invalidNel
            va.apply(
              "Function who?".invalidNel[Int => String]
            )("Not a number".invalidNel) shouldBe Invalid("Function who?" :: Nel("Not a number"))
          }
        }
      }
    }
  }
}

object ApplicativeBaseSpec {
  trait ApplicativeLike[F[_]] {
    def pure[A](a: A): F[A]
    def map2[A, B, C](fa: F[A], fb: F[B])(op: (A, B) => C): F[C]

    def map[A, B](fa: F[A])(op: A => B): F[B]
    def traverse[A, B](as: List[A])(op: A => F[B]): F[List[B]]
    def sequence[A](fas: List[F[A]]): F[List[A]]
    def replicateM[A](n: Int, fa: F[A]): F[List[A]]
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

    def apply[A, B](fop: F[A => B])(fa: F[A]): F[B]
  }

  type StringValidatedNel[V] = ValidatedNel[String, V]
}

object ApplicativeProxy extends ApplicativeLike[StringValidatedNel] {

  private val underlying = Applicative.Instances.validatedNelApplicative[String]

  override def pure[A](a: A): ValidatedNel[String, A] =
    underlying.pure   (a   )

  override def map2[A, B, C](fa: ValidatedNel[String, A], fb: ValidatedNel[String, B])(op: (A, B) => C): ValidatedNel[String, C] =
    underlying.map2         (fa                         , fb                         )(op             )

  override def map[A, B](fa: ValidatedNel[String, A])(op: A => B): ValidatedNel[String, B] =
    underlying.map      (fa                         )(op        )

  override def traverse[A, B](as: List[A])(op: A => ValidatedNel[String, B]): ValidatedNel[String, List[B]] =
    underlying.traverse      (as         )(op                              )

  override def sequence[A](fas: List[ValidatedNel[String, A]]): ValidatedNel[String, List[A]] =
    underlying.sequence   (fas                               )

  override def replicateM[A](n: Int, fa: ValidatedNel[String, A]): ValidatedNel[String, List[A]] =
    underlying.replicateM   (n     , fa                         )

  override def product[A, B](fa: ValidatedNel[String, A], fb: ValidatedNel[String, B]): ValidatedNel[String, (A, B)] =
    underlying.product      (fa                         , fb                         )

  override def apply[A, B](fop: ValidatedNel[String, A => B])(fa: ValidatedNel[String, A]): ValidatedNel[String, B] =
    underlying.apply      (fop                              )(fa                         )
}



object Applicative2Proxy extends ApplicativeLike[StringValidatedNel] {

  private val underlying = Applicative2.Instances.validatedNelApplicative2[String]

  override def pure[A](a: A): ValidatedNel[String, A] =
    underlying.pure   (a   )

  override def map2[A, B, C](fa: ValidatedNel[String, A], fb: ValidatedNel[String, B])(op: (A, B) => C): ValidatedNel[String, C] =
    underlying.map2         (fa                         , fb                         )(op             )

  override def map[A, B](fa: ValidatedNel[String, A])(op: A => B): ValidatedNel[String, B] =
    underlying.map      (fa                         )(op        )

  override def traverse[A, B](as: List[A])(op: A => ValidatedNel[String, B]): ValidatedNel[String, List[B]] =
    underlying.traverse      (as         )(op                              )

  override def sequence[A](fas: List[ValidatedNel[String, A]]): ValidatedNel[String, List[A]] =
    underlying.sequence   (fas                               )

  override def replicateM[A](n: Int, fa: ValidatedNel[String, A]): ValidatedNel[String, List[A]] =
    underlying.replicateM   (n     , fa                         )

  override def product[A, B](fa: ValidatedNel[String, A], fb: ValidatedNel[String, B]): ValidatedNel[String, (A, B)] =
    underlying.product      (fa                         , fb                         )

  override def apply[A, B](fop: ValidatedNel[String, A => B])(fa: ValidatedNel[String, A]): ValidatedNel[String, B] =
    underlying.apply      (fop                              )(fa                         )
}
