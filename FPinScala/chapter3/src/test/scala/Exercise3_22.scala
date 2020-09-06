import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_22 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(1,3,5,7,9)
  val ex2: List[Int] = List.apply(2,4,6,8,10) 

  // Exercise3.22: zipWithSumのテスト
  "zipWithSum関数" should "２つのリストの対応する部分を足し、リストを返す。" in {
    assert(List.zipWithSum(ex1,ex2) === List.apply(3,7,11,15,19))
  }

}