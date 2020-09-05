import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_11 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(2,4,5,19,10)
  val ex2: List[Int] = List.apply(1,10,12,16,20,30) 
  val ex3: List[Nothing] = Nil

  // Exercise3.11: sum関数(foldLeft)のテスト
  "sum3関数" should "Listの合計値を返す。" in {
    assert(List.sum3(ex1) === 40)
    assert(List.sum3(ex2) === 89)
  }

  it should "Nilの時は、0を返す。" in {
    assert(List.sum3(ex3) === 0)
  }

    // Exercise3.11: product関数(foldLeft)のテスト
  "product3関数" should "Listの積を返す。" in {
    assert(List.product3(ex1) === 7600)
    assert(List.product3(ex2) === 1152000)
  }

  it should "Nilの時は、0を返す。" in {
    assert(List.product3(ex3) === 1)
  }

    // Exercise3.11: length関数(foldLeft)のテスト
  "length2関数" should "Listの長さを返す。" in {
    assert(List.length2(ex1) === 5)
    assert(List.length2(ex2) === 6)
  }

  it should "Nilの時は、0を返す。" in {
    assert(List.length2(ex3) === 0)
  }

}