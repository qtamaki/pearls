package examples.ss4

import org.scalatest._

class SmallestSpec extends FlatSpec with Matchers {
  it should "Smallest0 function 1" in {
    val xs = List(1,2,3)
    val ys = List(4,5,6)
    0 to 5 foreach {x =>
      Smallest.smallest0(x, (xs, ys)) should be (x+1)
    }
    0 to 5 foreach {x =>
      Smallest.smallest0(x, (ys, xs)) should be (x+1)
    }
  }  

  it should "Smallest function 1" in {
    val xs = List(1,2,3)
    val ys = List(4,5,6)
    0 to 5 foreach {x =>
      Smallest.smallest(x, (xs, ys)) should be (x+1)
    }
    0 to 5 foreach {x =>
      Smallest.smallest(x, (ys, xs)) should be (x+1)
    }
  }

  it should "Smallest function 2" in {
    val xs = List(1,3,5)
    val ys = List(2,4,6)
    0 to 5 foreach {x =>
      Smallest.smallest(x, (xs, ys)) should be (x+1)
    }
    0 to 5 foreach {x =>
      Smallest.smallest(x, (ys, xs)) should be (x+1)
    }
  }  

//  it should "Smallest function long" in {
//    val x = 10000
//    Smallest.smallest(x, (Stream.from(0).take(x).toList, Stream.from(x).take(x).toList)) should be (x)
//  }

  it should "Smallest2 function 1" in {
    val xs = Vector(1,2,3)
    val ys = Vector(4,5,6)
    0 to 5 foreach {x =>
      Smallest.smallest2(x, (xs, ys)) should be (x+1)
    }
    0 to 5 foreach {x =>
      Smallest.smallest2(x, (ys, xs)) should be (x+1)
    }
  }  

  it should "Smallest2 function 2" in {
    val xs = Vector(1,3,5)
    val ys = Vector(2,4,6)
    0 to 5 foreach {x =>
      Smallest.smallest2(x, (xs, ys)) should be (x+1)
    }
    0 to 5 foreach {x =>
      Smallest.smallest2(x, (ys, xs)) should be (x+1)
    }
  }  
  
  it should "Smallest2 function long" in {
    val x = 10000
    Smallest.smallest2(x, (Stream.from(0).take(x).toVector, Stream.from(x).take(x).toVector)) should be (x)
  }
}