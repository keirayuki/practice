object exIf2 {
  /*
    非正格の例
      評価せずに渡したい引数の型の手前に => 矢印を付ける。
  */

    def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
      if (cond) onTrue else onFalse
  
    def main(args: Array[String]): Unit =
      // sys.errorは評価されない。
      println(if2(false, sys.error("fail!"), 3))
}
