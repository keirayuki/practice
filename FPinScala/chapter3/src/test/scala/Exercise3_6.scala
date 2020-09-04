import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_6 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(2,4,5,19,10)
  val ex2: List[Int] = List.apply(1,10,12,16,20,30) 
  val ex3: List[Nothing] = Nil

  // Exercise3.6: drop関数のテスト
  "init関数" should "Listの末尾を除くListを返す。" in {
    assert(List.init(ex1) === List.apply(2,4,5,19))
    assert(List.init(ex2) === List.apply(1,10,12,16,20))
  }

  it should "Nothing型の時は、Nilを返す。" in {
    assert(List.init(ex3) === Nil)
  }

}