package examples.ss5

object Sortsum2 {
  
  type Label = (Int,(Int,Int))
  type TaggedLabel = (Int,(Int,Int,Int))
  type LabeledArray = Map[(Int,Int,Int), Int]
    
  def sortsums(xs: List[Int], ys: List[Int]) = sortsubs(xs, ys.map(_ * -1)).map(_._1)

  def sortsubs(xs: List[Int], ys: List[Int]):List[Label] = subs(xs,ys).sortWith(cmp(mkArray(xs,ys)))
    
  def subs(xs: List[Int], ys: List[Int]): List[Label] = {
    for {
      (i, x) <- Stream.from(1).zip(xs).toList
      (j, y) <- Stream.from(1).zip(ys).toList
    } yield (x - y, (i,j))
  }
  
  def cmp(a:LabeledArray)(l1:Label, l2:Label): Boolean = (l1,l2) match {
    case ((x,(i,j)),(y,(k,l))) => a((1,i,k)) - a((2,j,l)) < 0
  }

  def mkArray(xs:List[Int], ys:List[Int]): LabeledArray = {
    println((xs,ys))
    val m = table(xs,ys).zip(Stream.from(1)).toMap
    println(m)
    m
  }
  
  def table(xs: List[Int], ys: List[Int]):List[(Int,Int,Int)] = {
    val xxs:List[TaggedLabel] = sortsubs2(xs).map(tag(1,_))
    val yys:List[TaggedLabel] = sortsubs2(ys).map(tag(2,_))
    merge(xxs, yys).map(_._2)
  } 

  def merge[A: Ordering](list1: List[A], list2: List[A]): List[A] = {
    (list1,list2) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x::xs, y::ys) => 
        if (implicitly[Ordering[A]].lt(x,y)) x :: merge(xs, y::ys)
        else y :: merge(x::xs, ys)
    }
  } 
  
  def tag(i:Int, x: Label): TaggedLabel = x match {case (x,(j,k)) => (x,(i,j,k))}
    //(x._1, (i,x._2._1, x._2._1))

  def switch:PartialFunction[Label,Label] = {case (x:Int,(i,j)) => (x * -1, (j,i))}
  
  def sortsubs2(ws: List[Int]):List[Label] = {
    lazy val m = ws.length / 2
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
}