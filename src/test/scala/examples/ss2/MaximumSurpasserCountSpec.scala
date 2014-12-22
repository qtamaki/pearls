package examples.ss2

import org.scalatest._
import org.scalacheck.Prop.forAll

class MaximumSurpasserCountSpec extends FlatSpec with Matchers {
  
  it should "Get max number 1" in {
    MaximumSurpasserCount.msc(List(1,2,3,4,5)) should be (4)
  }
  
  it should "Get number table 2" in {
    val prop = forAll { (s: String) =>
      MaximumSurpasserCount.table(s.toList).sorted should be (MaximumSurpasserCount.table2(s.toList))
      true
    }
    
  }
}