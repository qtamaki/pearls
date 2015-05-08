package examples.ss5

object Sortsum {

  type Label = (Int,(Int,Int))
  type TagedLabel = (Int,(Int,Int,Int))
    
  def sortsum(xs: List[Int], ys: List[Int]): List[Int] = 
    (for (x <- xs; y <- ys) yield x + y).sorted
  
  def subs(xs: List[Int], ys: List[Int]): List[Label] = {
    for {
      (i, x) <- Stream.from(1).zip(xs).toList
      (j, y) <- Stream.from(1).zip(ys).toList
    } yield (x - y, (i,j))
  }
  
  def sortsums(xs: List[Int], ys: List[Int]) = sortsubs(xs, ys.map(_ * -1)).map(_._1)
  
  def sortsubs(xs: List[Int], ys: List[Int]):List[Label] = subs(xs,ys).sorted
  
  def merge[A: Ordering](xs: List[A], ys: List[A]): List[A] = {
    val x :: xxs = xs
    val y :: yys = ys
    if (implicitly[Ordering[A]].lt(x,y)) x :: merge(xxs, ys)
    else y :: merge(xs, yys)
  } 
  
  def tag(i:Int, x: Label): TagedLabel = (x._1, (i,x._2._1, x._2._1))
      
  def table(xs: List[Int], ys: List[Int]):List[(Int,Int,Int)] = {
    val xxs:List[TagedLabel] = sortsubs(xs,xs).map(tag(1,_))
    val yys:List[TagedLabel] = sortsubs(ys,ys).map(tag(2,_))
    merge(xxs, yys).map(_._2)
  } 
}