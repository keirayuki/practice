import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_9 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(2,4,5,19,10)
  val ex2: List[Int] = List.apply(1,10,12,16,20,30) 
  val ex3: List[Nothing] = Nil

  // Exercise3.9: length関数(foldRight)のテスト
  "length関数" should "Listの長さを返す。" in {
    assert(List.length(ex1) === 5)
    assert(List.length(ex2) === 6)
  }

  it should "Nilの時は、0を返す。" in {
    assert(List.length(ex3) === 0)
  }

}