package casa

import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class FunctorSpec extends FreeSpec with FunctorLaws with GeneratorDrivenPropertyChecks {
  "Functor" - {
    "List is a functor" - {
      import Functor.Instances.listFunct

      "identity law" in {
        functorSatisfiesIdentityLaw[List, Int]
        functorSatisfiesIdentityLaw[List, String]
        functorSatisfiesIdentityLaw[List, Boolean]
        functorSatisfiesIdentityLaw[List, Double]
      }

      "composition law" in {
        functorSatisfiesCompositionLaw[List, Int, Int, Int]
        functorSatisfiesCompositionLaw[List, Int, String, Boolean]
        functorSatisfiesCompositionLaw[List, Double, String, Int]
        functorSatisfiesCompositionLaw[List, Double, Double, Double]
      }
    }
  }
}
