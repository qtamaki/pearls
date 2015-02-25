package examples.ss5

object Sortsum2 {
  
  type Label = (Int,(Int,Int))
  type TagedLabel = (Int,(Int,Int,Int))
  type LabeledArray = Map[(Int,Int,Int), Int]
    
  implicit def tagedLabelOrdering: Ordering[TagedLabel] = new Ordering[TagedLabel] {
    def compare(a:TagedLabel, b:TagedLabel) = (a,b) match {
      case ((i,(x,y,z)),(j,(l,m,n))) if i == j && x == l && y == m => z - n
      case ((i,(x,y,z)),(j,(l,m,n))) if i == j && x == l => y - m
      case ((i,(x,y,z)),(j,(l,m,n))) if i == j => x - l
      case _ => a._1 - b._1 
    }
  }
  
  def labelLt(a: Label, b:Label) = {
    (a,b) match {
      case ((i,(x,y)), (j,(l,m))) if i == j && x ==l => y < m
      case ((i,(x,y)), (j,(l,m))) if i == j => x < l
      case ((i,(x,y)), (j,(l,m))) => i < j
    }
  }

  def sortsums(xs: List[Int], ys: List[Int]) = sortsubs(xs, ys.map(_ * -1)).map(_._1)

  def sortsubs(xs: List[Int], ys: List[Int]):List[Label] = subs(xs,ys).sortWith(cmp(mkArray(xs,ys)))
    
  def subs(xs: List[Int], ys: List[Int]): List[Label] = {
    for {
      (i, x) <- Stream.from(1).zip(xs).toList
      (j, y) <- Stream.from(1).zip(ys).toList
    } yield (x - y, (i,j))
  }
  
  def cmp(a:LabeledArray)(l1:Label, l2:Label): Boolean = (l1,l2) match {
    case ((x,(i,j)),(y,(k,l))) => a((1,i,k)) - a((2,j,l)) > 0
  }

  def mkArray(xs:List[Int], ys:List[Int]): LabeledArray = ???
  
  def table(xs: List[Int], ys: List[Int]):List[(Int,Int,Int)] = {
    val xxs:List[TagedLabel] = sortsubs2(xs).map(tag(1,_))
    val yys:List[TagedLabel] = sortsubs2(ys).map(tag(2,_))
    merge(xxs, yys).map(_._2)
  } 

  def merge[A: Ordering](xs: List[A], ys: List[A]): List[A] = {
    val x :: xxs = xs
    val y :: yys = ys
    if (implicitly[Ordering[A]].lt(x,y)) x :: merge(xxs, ys)
    else y :: merge(xs, yys)
  } 
  
  def tag(i:Int, x: Label): TagedLabel = (x._1, (i,x._2._1, x._2._1))

  def switch:PartialFunction[Label,Label] = {case (x:Int,(i,j)) => (x * -1, (j,i))}
  
  def sortsubs2(ws: List[Int]):List[Label] = {
    lazy val m = ws.length % 2
    lazy val (xs,ys) = ws.splitAt(m)
    lazy val yys = sortsubs2(ys)
    lazy val xys = subs(xs,ys).sortWith(cmp(mkArray(xs,ys)))
    lazy val yxs = xys.reverse.map(switch)
    lazy val xxs = sortsubs2(xs)
    ws match {
      case Nil => Nil
      case w::Nil => List((w-w,(1,1)))
      case ws => val list = List(xys.map(incr(m, _)), yxs.map(incl(m, _)), yys.map(incb(m, _)))
                 list.foldRight(xxs)(merge)
    }
  }
  
  def incl(m:Int, a: Label): Label = {
    val (x, (i,j)) = a
    (x,(m+i,j))
  }
  
  def incr(m:Int, a: Label): Label = {
    val (x, (i,j)) = a
    (x,(i,m+j))
  }
  
  def incb(m:Int, a: Label): Label = {
    val (x, (i,j)) = a
    (x,(m+i,m+j))
  }
  
  def switch(a: Label): Label = {
    val (x, (i,j)) = a
    (x * -1,(i,j))
  }

  def merge(a: List[Label], b: List[Label]): List[Label] = {
    (a,b) match {
      case (Nil,ys) => ys
      case (xs, Nil) => xs
      case (x::xs, y::ys) => if (labelLt(x, y)) x :: merge(xs, (y::ys))
                             else y :: merge((x::xs), ys)
    }
  }
  
//  def compare(a:(Int,Int,Int), b:(Int,Int,Int)) = (a,b) match {
//    case ((a1,a2,a3),(b1,b2,b3)) if a1 == b1 && a2 == b2 => a3 - b3
//    case ((a1,a2,a3),(b1,b2,b3)) if a1 == b1 => a2 - b2
//    case ((a1,a2,a3),(b1,b2,b3)) => a1 - b1
//  }
  

}