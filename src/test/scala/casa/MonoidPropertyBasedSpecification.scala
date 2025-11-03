package casa

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop, Properties}

object MonoidPropertyBasedSpecification extends Properties("Monoid") {
  property("There is a monoid for Int where combine is addition and empty is 0") =
    forAll { (int1: Int, int2: Int) =>
      Monoid.Instances.intAdditionMonoid.combine(int1, int2) == int1 + int2
    }

  property("The addition monoid for Int satisfies monoid laws") =
    monoidLaws[Int](using Monoid.Instances.intAdditionMonoid, implicitly[Arbitrary[Int]])

  property("The concatenation monoid for List satisfies monoid laws") =
    monoidLaws[List[Int]](using Monoid.Instances.listConcatenationMonoid, implicitly[Arbitrary[List[Int]]]) &&
      monoidLaws[List[String]](using Monoid.Instances.listConcatenationMonoid, implicitly[Arbitrary[List[String]]]) &&
      monoidLaws[List[Unit]](using Monoid.Instances.listConcatenationMonoid, implicitly[Arbitrary[List[Unit]]])

  def monoidLaws[A](using Monoid[A], Arbitrary[A]): Prop = {
    SemigroupPropertyBasedSpecification.semigroupAssociativityLaw[A] :| "Monoid satisfies associativity law" &&
    monoidIdentityLaw[A] :| "Monoid satisfies identity law"
  }

  def monoidIdentityLaw[A](using monoid: Monoid[A], arb: Arbitrary[A]): Prop = {
    forAll { (a: A) =>
      monoid.combine(a, monoid.empty) == a &&
      monoid.combine(monoid.empty, a) == a
    }
  }
}
