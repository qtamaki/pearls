package examples.ss1

import org.scalatest._
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Gen
import scala.util.Random
import scala.collection.mutable.HashSet

class SmallestFreeNumberSpec extends FlatSpec with Matchers {
  val f1 = SmallestFreeNumber.minfree _
  val f2 = SmallestFreeNumber.minfree2 _
  val f3 = SmallestFreeNumber.minfree3 _
  val fs = List(f1,f2,f3)
  val lists = List((List(0, 1, 2, 3, 4, 6, 7, 8),5),
                   (List(8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6),15))  
  
  it should "Get smallest free number" in {
    for {
      f <- fs
      (list, ans) <- lists 
    } f(list) should be (ans)
  }
  it should "Check" in {
    val gen = Gen.resultOf { _: Int =>
    Random.shuffle(0 until 120 toList).take(100)
    /*
    (0 until 120).toList.sortBy(_ => Random.nextDouble()).take(100)
      var set = new HashSet[Int]()
      def fun: Int = {
        val x = Random.nextInt(120)
        if (set.contains(x)) {
          fun
        } else {
          set.add(x)
          x
        }
      }
      List.fill(100)(0).map(x => fun)
      */
    }
    def makeProp(f: List[Int] => Int) = forAll(gen) { (xs: List[Int]) =>
      val ans1 = f(xs)
      Stream.from(0).zip(xs.sorted).filter(x => x._1 != x._2) match {
        case Stream.Empty => true :| "ok"
        case noneEmpty =>
          val ans2 = noneEmpty.head._1
          ans1 should be <= xs.max
          ans1 should be(ans2)
          (ans1 == ans2) :| s"NGNG!! ${ans1} == ${ans2}"
      }
    }
    fs.foreach {f => makeProp(f).check}
  }
}
