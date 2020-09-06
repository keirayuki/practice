import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_20 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(1,2,3)

  // Exercise3.20: flatMapのテスト
  "flatMap関数" should "元のListの各要素に対しListを返し、それらは最終的なリストで返される" in {
    assert(List.flatMap(ex1)(i => List.apply(i,i)) === List.apply(1,1,2,2,3,3))
  }

}