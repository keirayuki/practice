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

    def append[A](a1: List[A], a2: List[A]): List[A] =
        a1 match {
            case Nil => a2
            case Cons(h,t) => Cons(h, append(t, a2))
        }

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

    /*
      Exercise3.4 :
       tailを一般化して、リストの先頭からn個の要素を削除するというdropという関数に書き換えよ。
    */
    def drop[A](as: List[A], n: Int): List[A] = (as,n) match {
        case (Nil, _) => Nil
        case (Cons(x,xs), 0) => Cons(x,xs)
        case (Cons(_, xs), _) => drop(xs, n-1)
    }

    def drop2[A](as: List[A], n: Int): List[A] = as match {
        case Nil => Nil
        case Cons(x,xs) if (n == 0) => Cons(x,xs)
        case Cons(_, xs) => drop2(xs,n-1)
    }

    /*
      Exercise3.5 :
       述語とマッチする場合に限り、Listからその要素までの要素を削除するdropWhileを実装せよ。
    */
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(x,xs) if (f(x)) => dropWhile(xs,f) 
        case _ => l
    }

    /*
      Exercise3.6 :
        Listの末尾を除く、全ての要素で構成されたListを返すinit関数を実装せよ。
    */
    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(h,Nil) => Nil
        case Cons(h,t) => Cons(h,init(t)) 
    }

}