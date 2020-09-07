import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_12 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(2,4,5)
  val ex2: List[Int] = List.apply(1,10,12,16,20,30) 
  val ex3: List[Nothing] = Nil

  // Exercise3.12: reverseのテスト
  "reverse関数" should "Listの要素を逆に並べたリストを返す。" in {
    assert(List.reverse(ex1) === List.apply(5,4,2))
    assert(List.reverse(ex2) === List.apply(30,20,16,12,10,1))
  }

  it should "Nilの時は、Nilを返す。" in {
    assert(List.reverse(ex3) === Nil)
  }

}