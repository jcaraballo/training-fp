package casa

import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class FunctorSpec extends FreeSpec with FunctorLaws with GeneratorDrivenPropertyChecks {
  "Functor" - {
    "List is a functor" in {
      import Functor.Instances.listFunct

      functorSatisfiesLaws[List, Int, Int, Int]
      functorSatisfiesLaws[List, Int, String, Boolean]
      functorSatisfiesLaws[List, Double, String, Int]
      functorSatisfiesLaws[List, Double, Double, Double]
    }
  }
}
