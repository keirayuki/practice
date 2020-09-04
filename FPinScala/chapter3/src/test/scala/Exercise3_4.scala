import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_4 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(2,4,10,19,5)
  val ex2: List[String] = List.apply("Hello","is","sweat","!") 
  val ex3: List[Nothing] = Nil

  // Exercise3.4: drop関数のテスト
  "drop関数" should "先頭から任意の数を削除できる。" in {
    assert(List.drop(ex1,5) === Nil)
    assert(List.drop(ex2,2) === List.apply("sweat","!"))
  }

  it should "Nothing型の時は、Nilを返す。" in {
    assert(List.drop(ex3,2) === Nil)
  }

  // Exercise3.4: drop2関数のテスト
  "drop2関数" should "先頭から任意の数を削除できる。" in {
    assert(List.drop2(ex1,5) === Nil)
    assert(List.drop2(ex2,2) === List.apply("sweat","!"))
  }

  it should "Nothing型の時は、Nilを返す。" in {
    assert(List.drop2(ex3,2) === Nil)
  }

}