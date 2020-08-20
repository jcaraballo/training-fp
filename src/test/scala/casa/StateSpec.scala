package casa

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class StateSpec extends AnyFreeSpec {
  "State" - {
    "primitives" - {
      "pure" in {
        State.pure[Int, Int](33).run(10) shouldBe (33, 10)
      }
      "get" in {
        State.get[Int].run(10) shouldBe (10, 10)
      }

      "set" in {
        State.set[Int](20).run(99) shouldBe ((), 20)
      }

      "modify" in {
        State.modify[Int](_ * 2).run(10) shouldBe ((), 20)
      }
    }

    "operations" - {
      "map" in {
        State.pure[Int, Int](1).map(_ * 2).run(10) shouldBe (2, 10)
      }

      "flatMap" in {
        State.pure[Int, Int](2).flatMap(a => State[Int, String](i => (a.toString, i * a))).run(10) shouldBe ("2", 20)
      }
    }
  }
}
