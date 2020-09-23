package state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
       (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  
  /* Exercise 6.1
  * RNG.nextIntを使って、0~Int.MaxValueのランダムな整数を生成する関数を記述せよ。
  * なお、Int.MinValueを返すときには、対応する自然数がないので、そのケースも対応する必要がある。
  */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i < 0) (-(i+1), r)
    else (i, r)
  }

  /* Exercise 6.2
  * 0~1(1を含まない)のDouble型の値を生成する関数を記述せよ。
  * Int.MaxValueを使って、星の整数の最大値を取得できることと、
  * x.toDoubleを使って、x: IntをDoubleに変換できる。
  */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i/ (Int.MaxValue.toDouble+1), r)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (i, rng2) => (i % 2 == 0, rng2)
    }

  /* Exercise 6.3
  * ペア（Int, Double)、ペア(Double, Int)、および3要素のタプル(Double, Double, Double)を
  * 生成する関数を記述せよ。既に作成済みの関数を再利用できる。
  */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /* Exercise 6.4
  * ランダムな整数のリストを生成する関数を記述せよ。
  */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (i, r) = rng.nextInt
      val (i2,r2) = ints(count-1)(r)
      (i :: i2, r2)
    }
    else (Nil, rng)
  }

  /* Exercise 6.5
  * mapを使ってdoubleをもう少し要領よく実装しなおせ
  */
  def doubleViaMap: Rand[Double] = 
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble+1))

  /* Exercise 6.6
  * map2を実装せよ。この関数は、raとrbの２つのアクションと、
  * それらの結果を結合する関数fを受け取り、それらを結合する新しいアクションを返す。
  */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
    val (i1,r1) = ra(rng)
    val (i2,r2) = rb(r1)
    (f(i1, i2), r2)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /* Exercise 6.7
  * 遷移のListを一つの繊維にまとめるためのsequence関数を実装せよ。
  * 標準らいぶらりList.fill(n)(x)関数を使って、xをn回繰り返すリストを作成できる。
  */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  // 0~n(nは含まない）の整数を生成する
  def nonNegativeLessThen(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThen(n)(rng)

  }

  /* Exercise 6.8
  * flatMapを実装し、それを使ってnonNegativeLessThanを実装せよ。
  */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (i, r) = f(rng)
    g(i)(r)
  }

  def nonNegativeLessThenViaflatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) {rng => (mod, rng)} else nonNegativeLessThenViaflatMap(n)
    }

  /* Exercise 6.9
  * flatMapを使ってmapとmap2を再実装せよ。
  */
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s){ i =>
      rng => (f(i), rng)
    }

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){ i => 
      flatMap(rb){i2 => 
         rng => (f(i,i2), rng)
      }
    }

}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = this match {
    case State(r) => State(
      x => r(x) match {
        case (a,s) => (f(a), s)
      }
    )
  }
    
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = this match {
    case State(r1) => State(
      x1 => r1(x1) match {
        case (a1, s1) => sb match {
          case State(r2) => r2(s1) match {
            case (a2, s2) => (f(a1, a2), s2)
          }
        }
      }
    )
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = this match {
    case State(r) => State(
      x => r(x) match {
        case (a, s) => f(a).run(s) 
      }
    )
  }
    
}

object State {
  type Rand[A] = State[RNG, A]

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }
}

