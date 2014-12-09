package examples.ss1

object SmallestFreeNumber {

  def minfree(xs: List[Int]): Int = {
    Stream.from(0).filter(x => !xs.contains(x)).head
  }

  def minfree2(xs: List[Int]): Int = {
    search(checklist(xs))
  }

  def search(xs: Array[Boolean]): Int = (xs.takeWhile { identity _ }).length

  def checklist(xs: List[Int]): Array[Boolean] = {
    val n = xs.length
    val ar = Array.fill(n + 1)(false)
    xs.filter(_ <= n).foreach { ar(_) = true }
    ar
  }

  def countlist(xs: List[Int]): Array[Int] = {
    val ar = Array.fill(xs.length)(0)
    xs.foreach { x => ar(x) = ar(x) + 1 }
    ar
  }

  def minfree3(xs: List[Int]): Int = minfrom(0, (xs.length, xs))

  def minfrom(a: Int, par: (Int, List[Int])): Int = {
    val (n, xs) = par
    val b = a + 1 + n / 2
    val (us, vs) = xs.partition { _ < b }
    val m = us.length
    
    if (n == 0) a
    else if (m == b - a) minfrom(b, (n - m, vs))
    else minfrom(a, (m, us))
  }

}