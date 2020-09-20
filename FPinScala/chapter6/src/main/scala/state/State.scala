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
  def mapViaMap: Rand[Double] = 
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble+1))
  }

