trait Either[+E, +A] {
    // Exercise 4.6
    // Right値を操作するmap関数をEitherに追加せよ。
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(v) => Right(f(v)) 
      case Left(e) => Left(e) 
    }

    // orElse関数をEitherに追加せよ。
    def orElse[EE >: E, B](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(v) => this
      case Left(e) => b
    }

    // flatMap関数をEitherに追加せよ
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(v) => f(v)
      case Left(e) => Left(e)
    }

    // map2関数をEitherに追加せよ。
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
      case Right(v) => Right(f(v,b))
      case Left(e) => Left(e) 
    }

    def map2_Answer[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
      for {
          aa <- this
          bb <- b
      } yield f(aa,bb)

    /* Exercise 4.7
        sequenceとtraverseを実装せよ。
    */
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
      case Nil => Right(Nil)
      case Left(e) :: _ => Left(e) 
      case h :: t =>  h flatMap (hh => sequence(t) map (hh :: _))
    }
      
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
      case Nil => Right(Nil)
      case h :: t =>  (f(h) map2 traverse(t)(f))(_ :: _)
    }
      
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]