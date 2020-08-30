import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_3 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(2,4,10,19,5)
  val ex2: List[Double] = List.apply(200.0,10.0,0.2,5.00,10000.0,90.0)
  val ex3: List[String] = List.apply("Hello","is","sweat","!") 
  val ex4: List[String] = List.apply("Python","is","Great","!!!!!")
  val ex5: List[Nothing] = Nil

  // Exercise3.3: setHead関数のテスト
  "setHead関数" should "Listの最初の要素を別の値と置き換える" in {
    assert(List.setHead(ex1,5) === List.apply(5,4,10,19,5))
    assert(List.setHead(ex2,0) === List.apply(0,10.0,0.2,5.00,10000.0,90.0))
    assert(List.setHead(ex3,"Sugar") === List.apply("Sugar","is","sweat","!"))
    assert(List.setHead(ex4,"Scala") === List.apply("Scala","is","Great","!!!!!"))
  }

  it should "Nothing型の時は、要素が1個であるListを作成" in {
    assert(List.setHead(ex5,"apple") === List.apply("apple"))
  }

}