package examples.ss6

object Komachi2 {

  type Digit = Int
  type Factor = List[Digit]
  type Term = List[Factor]
  type Expression = List[Term]
  type Digits = (Digit,Digit,Digit,Digit)

  def valExpr(ex: Expression): Int = ex.map(valTerm).foldLeft(0)(_+_)
  
  def valTerm(te: Term): Int = te.map(valFactor).foldLeft(1)(_*_)

  def valFactor(fa: Factor): Int = fa.tail.foldLeft(fa.head)((n:Int, d:Int) => 10 * n + d ) 
  
  def modify(x:Digit, ds:Digits): List[Digits] = {
    val (k,f,t,e) = ds
    List((10*k,k*x+f,t,e),(10,x,f*t,e),(10,x,1,f*t+e))
  }
  
  def good(c: Int, ds:Digits):Boolean = {
    val (k,f,t,e) = ds
    (f*t+e) == c
  }

  def ok(c: Int, ds:Digits):Boolean = {
    val (k,f,t,e) = ds
    (f*t+e) <= c
  }
  
  def solutions(c: Int, ds: List[Digit]): List[Expression] = 
    ds.foldRight[List[(Expression,Digits)]](Nil)((d,xs) => expand(c, d, xs)).filter((x) => good(c, x._2)).map(_._1)

  def expand(c:Int, x:Digit, xs: List[(Expression, Digits)]): List[(Expression, Digits)] = xs match {
    case Nil => List((List(List(List(x))), (10,x,1,0)))
    case evs => evs.flatMap((y) => glue(x, y).filter((z) => ok(c, z._2)))
  }
  
  def glue(x:Digit, ex: (Expression, Digits)): List[(Expression, Digits)] = ex match {
    case (((xs::xss)::xsss), (k,f,t,e)) => List(
                              (((x::xs)::xss)::xsss, (10*k,k*x+f,t,e)),
                              ((List(x)::xs::xss)::xsss, (10,x,f*t,e)),
                              (List(List(x))::(xs::xss)::xsss,(10,x,1,f*t+e)))
  }
}
