import errorhandling._


class Exercise4_3 {
    /*
     Exercise 4.3
      2項関数を使ってOption型の２つの値を結合する総称関数map2を記述せよ。
      どちらかのOption値がNoneの場合は、戻り値もNoneになる。
    */
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      (a,b) match {
        case (Some(a),Some(b)) => Some(f(a,b))
        case _ => None
      }
}
