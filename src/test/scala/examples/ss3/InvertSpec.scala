package examples.ss3

import org.scalatest._

class InvertSpec extends FlatSpec with Matchers {
    val f = (z: (Int,Int)) => z._1 * 3 + z._2 * 2
    val ans = List((0,10), (2,7), (4,4), (6,1))

    it should "Invert function 1" in {
      Invert.invert(f, 20) should be (ans)
    }

    it should "Invert function 2" in {
      Invert.invert2(f, 20) should be (ans)
    }

    it should "Invert function 3" in {
      Invert.invert3(f, 20) should be (ans)
    }

    it should "Invert function 4" in {
      Invert.invert4(f, 20) should be (ans)
    }

    it should "Invert function 5" in {
      Invert.invert5(f, 20) should be (ans)
    }

}