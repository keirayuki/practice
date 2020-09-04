import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_5 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(2,4,5,19,10)
  val ex2: List[Int] = List.apply(1,10,12,16,20,30) 
  val ex3: List[Nothing] = Nil

  // Exercise3.5: dropWhile関数のテスト
  "dropWhile関数" should "条件とマッチする間、その要素を削除する。" in {
    assert(List.dropWhile(ex1,(x: Int) => x <= 10) === List.apply(19,10))
    assert(List.dropWhile(ex2,(x: Int) => x <= 20) === List.apply(30))
  }

  it should "Nothing型の時は、Nilを返す。" in {
    assert(List.dropWhile(ex3,(x: Int) => x <= 2) === Nil)
  }

}