import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_24 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(1,2,3,4)
  val ex2: List[Int] = List.apply(2,3,10,26,1)

  // Exercise3.24: hasSubsequence
  "hasSubsequence関数" should "Listに別のリストがサブシーケンスとして含まれているかどうかを判定する。" in {
    assert(List.hasSubsequence(ex1,List.apply(1,2)) === true)
    assert(List.hasSubsequence(ex1,List.apply(2,3)) === true)
    assert(List.hasSubsequence(ex1,List.apply(1,2,3,4)) === true)
    assert(List.hasSubsequence(ex1,List.apply(1,3)) === false)
    assert(List.hasSubsequence(ex2,List.apply(2,3,10,26)) === true)
    assert(List.hasSubsequence(ex2,List.apply(10,26,1)) === true)
    assert(List.hasSubsequence(ex2,List.apply(2,3,26)) === false)
    assert(List.hasSubsequence(ex2,List.apply(1)) === true)
  }

}