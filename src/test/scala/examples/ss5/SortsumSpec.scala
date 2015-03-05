package examples.ss5

import org.scalatest._

class SortsumSpec extends FlatSpec with Matchers {
  it should "Sortsum function 1" in {
    Sortsum.sortsum(List(1,2,3), List(4,5,6)) should be (List(5, 6, 6, 7, 7, 7, 8, 8, 9))
  }
  it should "Sortsum function 2" in {
    Sortsum2.sortsums(List(1,2,3), List(4,5,6)) should be (List(5, 6, 6, 7, 7, 7, 8, 8, 9))
  }
}