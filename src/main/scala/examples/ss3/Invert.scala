package examples.ss3

object Invert {
  def invert(f: (Int,Int) => Int, z: Int): List[(Int, Int)] = { 
    for {
      x <- (0 to z)
      y <- (0 to z)
      if f(x,y) == z
    }yield {(x,y)}
  }.toList
}