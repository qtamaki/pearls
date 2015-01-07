package examples.ss3

import scala.annotation.tailrec

object Invert {
  type Fun = ((Int, Int)) => Int

  def invert(f: Fun, z: Int): List[(Int, Int)] = {
    for {
      x <- (0 to z)
      y <- (0 to z)
      if f(x, y) == z
    } yield { (x, y) }
  }.toList

  def invert2(f: Fun, z: Int): List[(Int, Int)] = {
    for {
      x <- (0 to z)
      y <- (0 to z - x)
      if f(x, y) == z
    } yield { (x, y) }
  }.toList

  def invert3(f: Fun, z: Int): List[(Int, Int)] = {
    def find(pair: (Int, Int), f: Fun, z: Int): List[(Int, Int)] = {
      val z2 = f(pair)
      val (u, v) = pair
      if (u > z || v < 0) List.empty
      else if (z2 < z) find((u + 1, v), f, z)
      else if (z2 == z) pair :: find((u + 1, v - 1), f, z)
      else find((u, v - 1), f, z)
    }
    find((0, z), f, z)
  }

  @tailrec
  def bsearch(g: Int => Int, pair: (Int, Int), z: Int): Int = {
    val (a, b) = pair
    val m = (a + b) / 2
    if (a + 1 == b) a
    else if (g(m) <= z) bsearch(g, (m, b), z)
    else bsearch(g, (a, m), z)
  }

  def invert4(f: Fun, z: Int): List[(Int, Int)] = {
    def find(pair: (Int, Int), f: Fun, z: Int): List[(Int, Int)] = {
      val z2 = f(pair)
      val (u, v) = pair
      val n = bsearch(x => f((x, 0)), (-1, z + 1), z)
      if (u > n || v < 0) List.empty
      else if (z2 < z) find((u + 1, v), f, z)
      else if (z2 == z) pair :: find((u + 1, v - 1), f, z)
      else find((u, v - 1), f, z)
    }
    val m = bsearch(y => f((0, y)), (-1, z + 1), z)
    find((0, m), f, z)
  }

  def invert5(f: Fun, z: Int): List[(Int, Int)] = {
    def find(pair1: (Int, Int), pair2: (Int, Int), f: Fun, z: Int): List[(Int, Int)] = {
      val (u, v) = pair1
      val (r, s) = pair2
      val p = (u + r) / 2
      val q = (v + s) / 2
      def rfind(p: Int): List[(Int, Int)] =
        (if (f((p, q)) == z) (p, q) :: find((u, v), (p - 1, q + 1), f, z)
        else find((u, v), (p, q + 1), f, z)) ++ find((p + 1, q - 1), (r, s), f, z)
      def cfind(q: Int): List[(Int, Int)] =
        find((u, v), (p - 1, q + 1), f, z) ++
          (if (f((p, q)) == z) (p, q) :: find((p + 1, q - 1), (r, s), f, z)
          else find((p + 1, q), (r, s), f, z))
      if (u > r || v < s) List.empty
      else if (v - s <= r - u) rfind(bsearch(x => f((x, q)), (u - 1, r + 1), z))
      else cfind(bsearch(y => f((p, y)), (s - 1, v + 1), z))
    }
    val m = bsearch(y => f((0, y)), (-1, z + 1), z)
    val n = bsearch(x => f((x, 0)), (-1, z + 1), z)
    find((0, m), (n, 0), f, z)
  }
}