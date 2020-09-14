package laziness

import Stream._

trait Stream[+A] {
  /*
  *  基本的に扱いは、List型と同じ。
  *  明示的なサンクを受け取る
  *  評価するときは、h()のように強制的に評価する
  *  なお、consの末尾tは評価しない。
  */
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /* Exercise 5.1
  * Streamをリストに変換し、それによりストリームを強制的に評価する関数を記述せよ。
  */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /* Exercise 5.2
  * Streamの先頭からn個の要素を取り出す関数take(n)と、
  * Streamの先頭からn個の要素をスキップするdrop(n)関数を記述せよ。
  */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 1) => cons(h(),t().take(n - 1))
    case Cons(h, t) if (n == 1) => cons(h(),empty)
    case _ => empty
  } 

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if (n > 0) => t().drop(n-1)
    case _ => this
  }

  /* Exercise 5.3
  * Streamの先頭から指定された述語とマッチする要素を全て取り出すtakeWhile関数を記述せよ。
  */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty 
  }


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  /*
  * スマートコンストラクタ
  * 追加の不変条件を満たすデータ型、あるいはパターンマッチングに使用される
  * 「本物」のコンストラクタとはシグネチャが少し異なるデータ型を生成する関数。
  */
  def cons[A](hd: => A, tl: =>Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // スマートコンストラクタ
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
