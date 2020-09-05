import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_14 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(2,4,5)
  val ex2: List[Int] = List.apply(1,10,12) 
  val ex3: List[Nothing] = Nil

  // Exercise3.14: append2のテスト
  "append2関数" should "2つのリストを連結してリストを返す。" in {
    assert(List.append2(ex1,ex2) === List.apply(2,4,5,1,10,12))
  }

  it should "連結対象がNilの時は、元のリストをそのまま返す。" in {
    assert(List.append2(ex1,ex3) === ex1)
  }

}