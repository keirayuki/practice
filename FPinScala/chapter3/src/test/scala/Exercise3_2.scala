import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_2 extends AnyFlatSpec with Diagrams {
  //Test用Listの定義
  val ex1: List[Int] = List.apply(2,4,10,19,5)
  val ex2: List[Int] = List.apply(8,4,2,100)
  val ex3: List[Double] = List.apply(200.0,10.0,0.2,5.00,10000.0,90.0)
  val ex4: List[String] = List.apply("Hello","Hello","World","!")  
  val ex5: List[Nothing] = Nil

  // Exercise3.3: tail関数のテスト
  "tail関数" should "最初の要素を削除する" in {
    assert(List.tail(ex1) === List.apply(4,10,19,5))
    assert(List.tail(ex2) === List.apply(4,2,100))
    assert(List.tail(ex3) === List.apply(10.0,0.2,5.00,10000.0,90.0))
    assert(List.tail(ex4) === List.apply("Hello","World","!"))
    assert(List.tail(ex5) === Nil)
  }

  it should "Nothing型はNothingを返す" in {
    assert(List.tail(ex5) === Nil)
  }

}
