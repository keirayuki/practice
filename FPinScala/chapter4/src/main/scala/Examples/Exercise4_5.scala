import errorhandling._


class Exercise4_5 {
  val ex4 = new Exercise4_3

  /*
    Exercise 4.5
      traverse関数を実装せよ。
  */
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case h :: t => f(h) flatMap (a => traverse(t)(f) map (a :: _))
  }

  def traverse_Answer[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case h :: t => ex4.map2(f(h), traverse_Answer(t)(f))(_ :: _)
  }
    
}
