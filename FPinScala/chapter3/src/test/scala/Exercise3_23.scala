import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_23 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(1,3,5,7,9)
  val ex2: List[Double] = List.apply(2.0,4.0,6.0,8.0,10.0) 

  // Exercise3.23: zipWithのテスト
  // Int + Double でも成り立つことを確認。
  "zipWith関数" should "２つのリストの対応する部分を足し、リストを返す。" in {
    assert(List.zipWith(ex1,ex2)((x,y) => x+y) === List.apply(3.0,7.0,11.0,15.0,19.0))
  }

}