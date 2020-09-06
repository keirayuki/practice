import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_17 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Double] = List.apply(2.0,4.8,5.2)
  val ex2: List[Double] = List.apply(1.0,10.1,12.2) 

  // Exercise3.17: add_oneのテスト
  "D_to_String関数" should "Double型の各要素をStringに直して、Stringのリストを返す。" in {
    assert(List.D_to_String(ex1) === List.apply("2.0","4.8","5.2"))
    assert(List.D_to_String(ex2) === List.apply("1.0","10.1","12.2"))
  }

}