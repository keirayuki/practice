import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_16 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(2,4,5)
  val ex2: List[Int] = List.apply(1,10,12) 

  // Exercise3.16: add_oneのテスト
  "add_one関数" should "各要素に１を足して、整数のリストを返す。" in {
    assert(List.add_one(ex1) === List.apply(3,5,6))
    assert(List.add_one(ex2) === List.apply(2,11,13))
  }

}