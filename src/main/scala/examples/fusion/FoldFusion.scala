package examples.fusion

object FoldFusion {

  def id[A](a: => A): A = a
  
  def const[A,B](a: => A)(b: => B): A = a
  
  def h(a: Int, b: Option[Int]): Option[Int] = {
    b match {
      case Some(x) => Some(a+x)
      case None => None 
    }
  }
  
  def sample(list: Traversable[Int]) {
    Some(list.foldRight(0)(_+_)) == list.foldRight[Option[Int]](Some(0))(h)
  }
}