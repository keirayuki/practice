import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_21 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(1,2,3,4,5,6,7,8)
  val ex2: List[Double] = List.apply(10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0) 

  // Exercise3.21: filter2のテスト
  "filter2関数" should "与えられたリストの奇数を全て削除する。" in {
    assert(List.filter2(ex1)(x => x%2 ==0) === List.apply(2,4,6,8))
    assert(List.filter2(ex2)(x => x % 2 == 0) === List.apply(10.0,12.0,14.0,16.0))
  }

}