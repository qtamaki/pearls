package examples.ss6

object Komachi {

  type Digit = Int
  type Factor = List[Digit]
  type Term = List[Factor]
  type Expression = List[Term]

  def valExpr(ex: Expression): Int = ex.map(valTerm).foldLeft(0)(_+_)
  
  def valTerm(te: Term): Int = te.map(valFactor).foldLeft(1)(_*_)

  def valFactor(fa: Factor): Int = fa.tail.foldLeft(fa.head)((n:Int, d:Int) => 10 * n + d ) 
  
  def good(v:Int):Boolean = v == 100
  
  //def expressions(ds: List[Digit]): List[Expression] = partitions(ds).flatMap(partitions)
  def expressions(ds: List[Digit]): List[Expression] = ds.foldRight[List[Expression]](Nil)(extend)

  def extend(x: Digit, exs: List[Expression]): List[Expression] = {
    exs match {
      case Nil => List(List(List(List(x))))
      case es => es.flatMap(glue(x, _))
    }
  }
  
  def glue(x:Digit, ex: Expression): List[Expression] = ex match {
    case ((xs::xss)::xsss) => List(((x::xs)::xss)::xsss,
                              (List(x)::xs::xss)::xsss,
                              List(List(x))::(xs::xss)::xsss)
  }
  
  def partitions[A](a: List[A]): List[List[List[A]]] = a match {
    case Nil => List(Nil)
    case x::xs => {
      lazy val ps = partitions(xs)
      val a = for(p <- ps) yield List(x)::p
      val b = for(p <- ps if p != Nil) yield {
        val ys = x :: p.head
        val yys = p.tail
        ys :: yys 
      }
      a ++ b
    }
  }
}
