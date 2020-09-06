import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_18 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(2,4,8,5)
  val ex2: List[Double] = List.apply(1.0,10.1,12.2) 

  // Exercise3.18: add_oneのテスト
  "map関数" should "各要素に１を足す及び文字列に変換し、それぞれのリストを返す。" in {
    assert(List.map(ex1)(x => x+1) === List.apply(3,5,9,6))
    assert(List.map(ex2)(x => x.toString()) === List.apply("1.0","10.1","12.2"))
  }

}