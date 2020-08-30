package datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x , xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs) 
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail:_*))

    /*
      Exercise3.2 :
       Listの最初の要素を削除する関数tailを実装せよ。
       ListがNilである場合、実装上の選択肢として他に何があるか。
    */
    def tail[A](as: List[A]): List[A] = as match {
        case Nil => Nil
        case Cons(_, xs) => xs 
    }

    /*
      Exercise3.3 :
       Listの最初の要素を別の値と置き換えるsedHead関数を実装せよ。
    */
    def setHead[A](as: List[A], i: A): List[A] = as match {
        case Nil => Cons(i,Nil)
        case Cons(_, xs) => Cons(i, xs) 
    }


}