package errorhandling

trait Option[+A]{
    // Exercise 4.1
    /*
       map関数をOptionで実装せよ。
       OptionがNone出ない場合は関数を適用。
    */
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(get) => Some(f(get))
    }

    /*
       flatMap関数をOptionで実装せよ。
       OptionがNoneでない場合は、失敗する可能性があるfを適用。
    */
    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

    /*
       getOrElse関数をOptionで実装せよ。
       OptionのSomeケースの結果を返す。Noneの場合は、default値をかえす。
    */
    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(get) => get
    }

    /*
       orElse関数をOptionで実装せよ。
       １つ目のOptionが定義されている場合はそれを返し、
       そうでない場合は２つ目のOptionを返す。
    */
    def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob 

    /*
       filter関数をOptionで実装せよ。
       値がfの条件を満たさない場合は、SomeをNoneに変換
    */
    def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)

    /*
     Exercise 4.2
       flatMapをベースとして、variance関数を実装せよ。
       シーケンスの平均をm,シーケンスの各要素をxとすれば、
       分散はmath.pow(x-m, 2)の平均である。
    */
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = 
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

