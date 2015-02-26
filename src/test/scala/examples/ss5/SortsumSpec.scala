package examples.ss5

import org.scalatest._

class SortsumSpec extends FlatSpec with Matchers {
  it should "Sortsum function 1" in {
    Sortsum.sortsum(List(1,2,3), List(4,5,6))
  }
}