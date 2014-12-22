package examples.ss2

object MaximumSurpasserCount {
  
  def msc[A : Ordering](xs: List[A]): Int = 
    xs.tails.collect({case (z::zs) => scount(z,zs)}).max
  
  def scount[A : Ordering](x: A, xs: List[A]): Int = 
    xs.filter(y => implicitly[Ordering[A]].lt(x, y)).size
  
  def table[A : Ordering](xs: List[A]): List[(A, Int)] = 
    xs.tails.collect({case (z::zs) => (z, scount(z, zs))}).toList
  
  def join[A : Ordering](txs: List[(A, Int)], tys: List[(A, Int)]): List[(A, Int)] =
    txs.collect({ case (z, c) => (z, c + tcount(z, tys))}) ++ tys
  
  def tcount[A : Ordering](z: A, tys: List[(A, Int)]): Int = 
    scount(z, tys.map(_._1))

  def tcount2[A : Ordering](z: A, tys: List[(A, Int)]): Int =
    tys.dropWhile(x => implicitly[Ordering[A]].lteq(x._1, z)).length
    
  def join2[A : Ordering](n: Int, txs: List[(A, Int)], tys: List[(A, Int)]): List[(A, Int)] = (n, txs, tys) match {
    case (0, txs, Nil) => txs
    case (_, Nil, tys) => tys
    case (n, txs@((x,c) :: txs2), tys@((y,d) :: tys2)) =>
      if (implicitly[Ordering[A]].lt(x, y)) (x,c + n) :: join2(n, txs2, tys)
      else (y, d) :: join2(n-1, txs, tys2)
    case _ => ???
  }
    
  def table2[A : Ordering](xs: List[A]): List[(A, Int)] = xs match { 
    case List() => List.empty
    case List(x) => List((x, 0))
    case xs => 
      val m = xs.length
      val n = m / 2
      val (ys, zs) = xs.splitAt(n)
      join2(m - n, table2(ys), table2(zs))  
  }
    
}