package casa

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.shouldBe

class MonadSpec extends AnyFreeSpec {
  private def asPair[A, B]: (A, B) => (A, B) = (_, _)

  private val toMaybeInt: String => Option[Int] = s => try {
    Some(s.toInt)
  } catch {
    case _: NumberFormatException => None
  }

  "Monad" - {
    "List is a monad" - {
      import Monad.Instances.listMonad

      "map2" in {
        listMonad.map2(List(1, 2, 3), List("a", "b"))(asPair) shouldBe List(
          (1, "a"), (1, "b"),
          (2, "a"), (2, "b"),
          (3, "a"), (3, "b")
        )
        listMonad.map2(List(1, 2, 3), Nil)(asPair) shouldBe Nil
        listMonad.map2(Nil, List("a", "b"))(asPair) shouldBe Nil
        listMonad.map2(Nil, Nil)(asPair) shouldBe Nil
      }

      "sequence" in {
        listMonad.sequence(List(List(1, 2, 3), List(40, 50))) shouldBe List(
          List(1, 40), List(1, 50),
          List(2, 40), List(2, 50),
          List(3, 40), List(3, 50)
        )
        listMonad.sequence(List(List(1, 2, 3), Nil)) shouldBe Nil
        listMonad.sequence(List(Nil, List(40, 50))) shouldBe Nil
        listMonad.sequence(List(Nil, Nil)) shouldBe Nil
      }

      "traverse" in {
        listMonad.traverse(List("foo", "bar"))(_.toList) shouldBe List(
          List('f', 'b'), List('f', 'a'), List('f', 'r'),
          List('o', 'b'), List('o', 'a'), List('o', 'r'),
          List('o', 'b'), List('o', 'a'), List('o', 'r')
        )
        listMonad.traverse(List("foo", ""))(_.toList) shouldBe Nil
        listMonad.traverse(List("", "bar"))(_.toList) shouldBe Nil
        listMonad.traverse(List("", ""))(_.toList) shouldBe Nil
      }

      "replicateM" in {
        listMonad.replicateM(3, List("foo")) shouldBe List(List("foo", "foo", "foo"))
        listMonad.replicateM(3, Nil) shouldBe Nil
      }

      "product" in {
        listMonad.product(List(1, 2, 3), List(40, 50)) shouldBe List(
          (1, 40), (1, 50),
          (2, 40), (2, 50),
          (3, 40), (3, 50)
        )
        listMonad.product(List(1, 2, 3), Nil) shouldBe Nil
        listMonad.product(Nil, List(40, 50)) shouldBe Nil
        listMonad.product(Nil, Nil) shouldBe Nil
      }

      "Kleisli composition" in {
        val itSelfAndReverse : String => List[String] = s => if (s.isEmpty) Nil else List(s, s.reverse)
        val stringToItsDigits: String => List[Int]    = _.toList.flatMap(c => toMaybeInt(c.toString))

        listMonad.compose(itSelfAndReverse, stringToItsDigits)("12three") shouldBe List(
          1, 2, 2, 1
        )
        listMonad.compose(itSelfAndReverse, stringToItsDigits)("") shouldBe Nil
        listMonad.compose(itSelfAndReverse, stringToItsDigits)("foo") shouldBe Nil
      }
    }

    "Option is a monad" - {
      import Monad.Instances.optionMonad

      "map2" in {
        optionMonad.map2(Some(1), Some("a"))(asPair) shouldBe Some((1, "a"))
        optionMonad.map2(Some(1), None)(asPair) shouldBe None
        optionMonad.map2(None, Some("a"))(asPair) shouldBe None
        optionMonad.map2(None, None)(asPair) shouldBe None
      }

      "sequence" in {
        optionMonad.sequence(List(Some(1), Some(2))) shouldBe Some(List(1, 2))
        optionMonad.sequence(List(Some(1), None)) shouldBe None
        optionMonad.sequence(List(None, Some(2))) shouldBe None
        optionMonad.sequence(List(None, None)) shouldBe None
      }

      "traverse" in {
        optionMonad.traverse(List("1", "2"))(toMaybeInt) shouldBe Some(List(1, 2))
        optionMonad.traverse(List("1", "bar"))(toMaybeInt) shouldBe None
        optionMonad.traverse(List("foo", "2"))(toMaybeInt) shouldBe None
        optionMonad.traverse(List("foo", "bar"))(toMaybeInt) shouldBe None
      }

      "replicateM" in {
        optionMonad.replicateM(3, Some(1)) shouldBe Some(List(1, 1, 1))
        optionMonad.replicateM(3, None) shouldBe None
      }

      "product" in {
        optionMonad.product(Some(1), Some(2)) shouldBe Some(1, 2)
        optionMonad.product(Some(1), None) shouldBe None
        optionMonad.product(None, Some(2)) shouldBe None
        optionMonad.product(None, None) shouldBe None
      }

      "Kleisli composition" in {
        val withoutBraces: String => Option[String] = { s =>
          if (s.startsWith("[") && s.endsWith("]"))
            Some(s.substring(1, s.length - 1))
          else
            None
        }
        optionMonad.compose(withoutBraces, toMaybeInt)("[1]") shouldBe Some(1)
        optionMonad.compose(withoutBraces, toMaybeInt)("1") shouldBe None
        optionMonad.compose(withoutBraces, toMaybeInt)("[foo]") shouldBe None
      }
    }

    "Either is a monad" - {
      import Monad.Instances.eitherMonad

      val toErrorOrInt: String => Either[String, Int] = s => try {
        Right(s.toInt)
      } catch {
        case _: NumberFormatException => Left(s"'$s' is not an Int")
      }

      "map2" in {
        eitherMonad.map2(Right(1), Right("a"))(asPair) shouldBe Right((1, "a"))
        eitherMonad.map2(Right(1), Left("Booh!"))(asPair) shouldBe Left("Booh!")
        eitherMonad.map2(Left("Booh!"), Right("a"))(asPair) shouldBe Left("Booh!")
        eitherMonad.map2(Left("Booh!"), Left("Baah!"))(asPair) shouldBe Left("Booh!")
      }

      "sequence" in {
        eitherMonad.sequence(List(Right(1), Right(2))) shouldBe Right(List(1, 2))
        eitherMonad.sequence(List(Right(1), Left("Baah!"))) shouldBe Left("Baah!")
        eitherMonad.sequence(List(Left("Booh!"), Right(2))) shouldBe Left("Booh!")
        eitherMonad.sequence(List(Left("Booh!"), Left("Baah!"))) shouldBe Left("Booh!")
      }

      "traverse" in {
        eitherMonad.traverse(List("1", "2"))(toErrorOrInt) shouldBe Right(List(1, 2))
        eitherMonad.traverse(List("1", "bar"))(toErrorOrInt) shouldBe Left("'bar' is not an Int")
        eitherMonad.traverse(List("foo", "2"))(toErrorOrInt) shouldBe Left("'foo' is not an Int")
        eitherMonad.traverse(List("foo", "bar"))(toErrorOrInt) shouldBe Left("'foo' is not an Int")
      }

      "replicateM" in {
        eitherMonad.replicateM(3, Right(1)) shouldBe Right(List(1, 1, 1))
        eitherMonad.replicateM(3, Left("foo")) shouldBe Left("foo")
      }

      "product" in {
        eitherMonad.product(Right(1), Right(2)) shouldBe Right(1, 2)
        eitherMonad.product(Right(1), Left("Baah!")) shouldBe Left("Baah!")
        eitherMonad.product(Left("Booh!"), Right(2)) shouldBe Left("Booh!")
        eitherMonad.product(Left("Booh!"), Left("Baah!")) shouldBe Left("Booh!")
      }

      "Kleisli composition" in {
        val withoutBraces: String => Either[String, String] = { s =>
          if (!s.startsWith("["))
            Left(s"'$s' doesn't start with '['")
          else if (!s.endsWith("]"))
            Left(s"'$s' doesn't end with '['")
          else
            Right(s.substring(1, s.length - 1))
        }
        eitherMonad.compose(withoutBraces, toErrorOrInt)("[1]") shouldBe Right(1)
        eitherMonad.compose(withoutBraces, toErrorOrInt)("1") shouldBe Left("'1' doesn't start with '['")
        eitherMonad.compose(withoutBraces, toErrorOrInt)("[foo]") shouldBe Left("'foo' is not an Int")
      }
    }

    "State is a monad" - {
      import Monad.Instances.stateMonad

      case class Ship(fuel: Int, position: Position)
      case class Position(x: Int, y: Int)
      sealed abstract class Direction {
        def move: Int => Position => Position
      }
      case object Left extends Direction {
        def move: Int => Position => Position = steps => {
          case Position(x, y) => Position(x - steps, y)
        }
      }
      case object Up extends Direction {
        def move: Int => Position => Position = steps => {
          case Position(x, y) => Position(x, y + steps)
        }
      }

      def move(direction: Direction)(steps: Int): State[Ship, Int] = for {
        ship <- State.get[Ship]
        actualSteps = Math.min(ship.fuel, steps)
        newShip = Ship(ship.fuel - actualSteps, direction.move(actualSteps)(ship.position))
        _ <- State.set(newShip)
      } yield newShip.fuel


      "operations" - {
        "map2" in {
          stateMonad[Int].map2(State.pure(1), State.pure("a"))(asPair).run(99) shouldBe((1, "a"), 99)

          // Modifies the state sequentially
          stateMonad[Int].map2(State.modify(_ * 2), State.modify(_ + 5))(asPair).run(10) shouldBe(((), ()), 25)
          stateMonad[Int].map2(State.modify(_ + 5), State.modify(_ * 2))(asPair).run(10) shouldBe(((), ()), 30)
        }

        "sequence" in {
          stateMonad.sequence(List[State[Int, Int]](State.pure(1), State.pure(2))).run(99) shouldBe(List(1, 2), 99)

          // Modifies the state sequentially
          stateMonad.sequence(List[State[Int, Unit]](State.modify(_ * 2), State.modify(_ + 5))).run(10) shouldBe(List((), ()), 25)
          stateMonad.sequence(List[State[Int, Unit]](State.modify(_ + 5), State.modify(_ * 2))).run(10) shouldBe(List((), ()), 30)
        }

        "traverse" in {
          stateMonad[Int].traverse(List("1", "2"))(stateMonad.pure).run(99) shouldBe(List("1", "2"), 99)

          // Modifies the state sequentially
          stateMonad[String].traverse(List("foo", "bar"))(s => State.modify(state => s"$s + ($state)")).run(".") shouldBe(List((), ()), "bar + (foo + (.))")
          stateMonad[String].traverse(List("bar", "foo"))(s => State.modify(state => s"$s + ($state)")).run(".") shouldBe(List((), ()), "foo + (bar + (.))")
        }

        "replicateM" in {
          val moveThreeUp = stateMonad[Ship].replicateM(3, move(Up)(1))
          moveThreeUp.run(Ship(fuel = 1, position = Position(0, 0))) shouldBe(List(0, 0, 0), Ship(0, Position(0, 1)))
          // Note how replicateM replicates the State monad, and not just the outcome of running it. E.g. if we had
          // implemented it as `map(fa)(List.fill(n)(_))`, the state manipulation would have only occurred once; the
          // result would have been: (List(9, 9, 9), Ship(9, Position(0, 1)))
          moveThreeUp.run(Ship(fuel = 10, position = Position(0, 0))) shouldBe(List(9, 8, 7), Ship(7, Position(0, 3)))
        }

        "product" in {
          stateMonad[String].product(State.pure(1), State.pure(2)).run("whatever") shouldBe((1, 2), "whatever")

          // Modifies the state sequentially
          stateMonad[String].product(State.modify(_ + "1"), State.modify(_ + 2)).run(".") shouldBe(((), ()), ".12")
          stateMonad[String].product(State.modify(_ + "2"), State.modify(_ + 1)).run(".") shouldBe(((), ()), ".21")
        }

        "Kleisli composition" in {
          val upAndAllLeft: Int => State[Ship, Int] = stateMonad[Ship].compose[Int, Int, Int](
            i => move(Up)(i),
            fuelLeft => move(Left)(fuelLeft)
          )

          upAndAllLeft(1).run(Ship(fuel = 10, position = Position(0, 0))) shouldBe(0, Ship(fuel = 0, position = Position(-9, 1)))
        }
      }
    }
  }
}
