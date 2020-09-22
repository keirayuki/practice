package parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = 
    (es: ExecutorService) => UnitFuture(a)
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  //タイムアウト考慮しない
  def map2Before[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
    
  //タイムアウトを考慮する
  /* Exercise 7.3
  * map2の実装を修正し、Futureのタイムアウトの規約に従うようにせよ。
  */
  private case class Map2Future[A,B,C](a : Future[A], b : Future[B], f : (A,B) => C) extends Future[C]{
    def isDone = a.isDone && b.isDone
    def get() : C = f(a.get, b.get)

    def get(timeout : Long, units : TimeUnit) : C = {
      val start = System.currentTimeMillis()
      val aRet = a.get(timeout, units)
      val bRet = b.get(TimeUnit.MILLISECONDS.convert(timeout, units) - (System.currentTimeMillis() - start), TimeUnit.MILLISECONDS)
      f(aRet, bRet)
    }

    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning:Boolean) : Boolean = a.cancel(evenIfRunning) && b.cancel(evenIfRunning)
  }

  def map2After[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
      (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A]{
      def call = a(es).get
    })

  /* Exercise 7.4
  * lazyUnitを使ってasyncF関数を記述せよ・
  * この関数は、任意の関数 A => B から、その結果を非同期で評価する関数へと変換する。
  */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] =
    x => lazyUnit(f(x))

  //Par[List[Int]]の中身をソートする
  def sortParBefore(parList: Par[List[Int]]): Par[List[Int]] =
    map2Before(parList, unit(()))((a, _) => a.sorted)

  // A => B型 の任意の関数を Par[A] => Par[B]にする。
  // sortParを一般化したもの。
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2Before(pa, unit(()))((a, _) => f(a))

  def sortParAfter(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  /* Exercise 7.5
  * sequenceという関数を記述せよ。
  * 追加のプリミティブは必要はない、runを呼び出さないこと。
  */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case h :: t => map2Before(h, fork(sequence(t)))(_ :: _)
      case Nil =>  unit(Nil)
    }
  
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /* Exercise 7.6
  * リストの要素を並行してフィルタリングするparFilterを実装せよ。
  */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = 
    map(parMap(as)(x => if (f(x)) List(x) else List()))(_.flatten)

  //最初の計算の結果に基づいて、フォークする計算のどちらかを関数に選択する関数
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = 
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)
  
  /* Exercise 7.11
  * choiceNを実装し、choiceNをベースにchoiceを実装せよ。
  */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es =>
      val num = run(es)(n).get
      run(es)(choices(num))
  }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = 
    choiceN(map(cond)(x => if (x == true) 0 else 1))(List(t,f))

  /* Exercise 7.12
  * choiceMapを実装せよ。
  */
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = {
    es =>
      val k = run(es)(key).get
      run(es)(choices(k))
  }

  /* Exercise 7.13
  * chooserを実装し、それを使ってchoiceとchoiceNを実装せよ。
  * chooserはflatMapという名前でしばしば呼ばれる
  */
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es =>
      val k = run(es)(pa).get
      run(es)(choices(k))
  }

  def choiceViaChooser[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(p)(x => if (x) t else f)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = 
    chooser(n)(i => choices(i))

  /* Exercise 7.14
  * joinを実装せよ。
  * joinを使って、flatMapを実装できるか、またflatMapを使ってjoinを実装できるか。
  */
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    chooser(a)(x => x)

  def flatMapViaJoin[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))
    

  
}