```Scala
object ex7 extends App { 
  // Sum関数
  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a, b) => a + b)

  /* 上記のSum関数を並列化したい
  　　分割統治アルゴリズムを使ってリストを合計
  　　＝並列化が可能
  */
  def sumS(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      sum(l) + sum(r)
    }
  
  /* 
  　　sum(l) + sum(r)を、見ると
  　・データ型が結果を保持でき、その結果は有意議型の値となる
  　　例えば、sum(l) = sum((1,2)) = sum((1)) + sum((2))としたとき、
  　　sum((1))は結果を保持していて（今回の場合は1)、Intを返す。
  　・その結果を取得する方法が必要
  　　上記の場合、sum((1))から結果を取り出す方法が必要
  */

  //上記を並列設計に適用し、以下の関数を定義
  def unit[A](a: => A): Par[A]
  /*
  　評価されてないAを受け取り、それを別のスレッドで評価するための計算を返す。
  */

  def get[A](a: Par[A]): A 
  /*
  　並列計算から結果の値を取り出す。
  */

  def sumP(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else{
      val (l, r) = ints.splitAt(ints.length/2)
      val sumL: Par[Int] = unit(sum(l))
      val sumR: Par[Int] = unit(sum(r))
      get(sumL) + get(sumR)
    }
  
  /* 
  * 上記は副作用がある
  * unitが引数の評価を直ちに開始した場合、その評価が完了するまでgetが待機する。
  * unitがPar[int]を返すだけで、非同期計算となり、同時に実行されない
  * したがって、getを避けるか少なくとも最後の最後まで呼び出しを遅らせる必要がある。
  */

  def sumP2(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      map2(sum(l), sum(r))(_ + _)
    }

  /* Exercise 7.1
  * Par.map2は２つの並列計算の結果を結合する新しい高階関数である。
  * そのシグネチャはどのようになるか。
  */
  def map2[A,B,C](p1: Par[A], p2: Par[B])(f: (A, B) => C ): Par[C]
  
}
```
