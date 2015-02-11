package examples.ss4

import org.scalatest._

class SmallestSpec extends FlatSpec with Matchers {
  it should "Smallest function 1" in {
    Smallest.smallest(3, (List(1,2,3), List(4,5,6))) should be (4)
  }  

}