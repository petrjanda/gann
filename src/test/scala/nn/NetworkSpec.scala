package nn

import org.scalatest.{FunSpec}
import org.scalatest.ShouldMatchers

class NetworkSpec extends FunSpec with ShouldMatchers {
  describe("NetworkSpec.size") {
    it("4,5,4 -> 44") {
      val counts = List(4,5,4)

      Network.size(counts) should be === 44
    }

    it("1,2,1 -> 5") {
      val counts = List(1,2,1)

      Network.size(counts) should be === 5
    }
  }
}
