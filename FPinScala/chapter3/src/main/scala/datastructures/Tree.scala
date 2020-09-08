package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    /*
      Exercise3.25
       ２分木のノード（LeafとBranch）の数を数えるsize関数を記述せよ。
    */
    def size[A](t: Tree[A]): Int = t match {
        case Leaf(v) => 1
        case Branch(l,r) =>  1 + size(l) + size(r)
    }

    /*
      Exercise3.26
       Tree[Int]の最大の要素を返すmaximum関数を記述せよ。
    */
    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(v) => v 
        case Branch(l,r) => maximum(l) max maximum(r)
    }

    /*
      Exercise3.27
       ２分木のルートから任意のLeafまでの最長パスを返すdepth関数を記述せよ。
    */
    def depth[A](t: Tree[A]): Int = t match {
        case Leaf(v) => 0
        case Branch(l,r) => 1 + (depth(l) max depth(r))
    }

    /*
      Exercise3.28
       ２分木の各要素を特定の関数を使って変更するmap関数を記述せよ。
    */
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l,r) => Branch(map(l)(f),map(r)(f))
    }

    /*
      Exercise3.29
       size、maximum、depth、mapを一般化し、それらの類似点を抽象化する
       新しいfold関数を記述せよ。
       そして、fold関数を用いてそれらの関数を実装せよ。
    */
    def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
        case Leaf(v) => f(v)
        case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def size2[A](t: Tree[A]): Int =
        fold(t)(v => 1)((l,r) => 1 + l + r)

    def maximum2(t: Tree[Int]): Int =
        fold(t)(v => v)((l,r) => l max r)
    
    def depth2[A](t: Tree[A]): Int =
        fold(t)(v => 0)((l,r) => 1 + (l max r))
    
    def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
        fold(t)(v => Leaf(f(v)): Tree[B])((l,r) => Branch(l,r))
}