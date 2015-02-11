package examples.ss4

object Smallest {
  
  def smallest0[A:Ordering](k: Int, pair: (List[A],List[A])):A = {
    def union(pair:(List[A], List[A])): List[A] = {
      pair match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x::xs, y::ys) => if (implicitly[Ordering[A]].lt(x,y)) x :: union (xs, y::ys)
                               else y :: union (x::xs, ys)
      }
    }
    union(pair)(k)
  }
  
  def smallest[A: Ordering](k: Int, pair: (List[A],List[A])):A = {
    pair match {
      case (Nil, ws) => ws(k)
      case (zs, Nil) => zs(k)
      case (zs, ws) =>
        val p = zs.length / 2
        val q = ws.length / 2
        val (xs,a::ys) = zs.splitAt(p)
        val (us,b::vs) = ws.splitAt(q)
        (implicitly[Ordering[A]].lt(a,b), k <= p + q) match {
          case (true,true)   => smallest(k, (zs,us))
          case (true,false)  => smallest(k-p-1, (ys,ws))
          case (false,true)  => smallest(k, (xs,ws))
          case (false,false) => smallest(k-q-1, (zs, vs))
        }
    }
  }
  
  def search[A: Ordering](pair: (Vector[A],Vector[A]), k:Int, pairX:(Int,Int), pairY:(Int,Int)): A = {
    val (xa,ya) = pair
    val (lx,rx) = pairX
    val (ly,ry) = pairY
    val mx = (lx + rx) / 2
    val my = (ly + ry) / 2
    if (lx == rx) ya(k + ly)
    else if (ly == ry) xa(k + lx)
    else (implicitly[Ordering[A]].lt(xa(mx), ya(my)), k <= mx - lx + my - ly) match {
      case (true, true)    => search(pair, k, (lx,rx), (ly,my))
      case (true, false)   => search(pair, k - (mx - lx) - 1, (mx + 1, rx), (ly, ry))
      case (false, true)   => search(pair, k, (lx, mx), (ly, ry))
      case (false , false) => search(pair, k - (my - ly) - 1, (lx,rx), (my + 1, ry))
    }
  }
  
  def smallest2[A: Ordering](k: Int, pair: (Vector[A],Vector[A])):A = {
    val (xa,ya) = pair
    search(pair, k, (0, xa.length), (0, ya.length))
  }
}