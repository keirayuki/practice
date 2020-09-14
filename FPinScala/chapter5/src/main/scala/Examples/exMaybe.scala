object exMaybe {
  /*
    lazyの使い道
      結果を一回だけ評価したい場合、lazyをつける。
      lazy val宣言した変数は、最初に参照される時まで先送りされる。  
      その後の参照で評価を繰り返し行うことがないように結果はキャッシュされる。
  */

    def maybeTwice(b: Boolean, i: => Int) = 
      if(b) i+i else 0
    
    def maybeTwice2(b: Boolean, i: => Int) = {
      lazy val j = i
      if(b) j+j else 0
    }
  
    def main(args: Array[String]): Unit = {
      // iが２回参照される
      val x = maybeTwice(true, { println("hi"); 1+41})
      // iは1回のみ参照される
      val x2 = maybeTwice2(true, { println("hi"); 1+41})
    }

}
