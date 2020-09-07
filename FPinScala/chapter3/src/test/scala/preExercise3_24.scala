import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class preExercise3_24 extends AnyFlatSpec with Diagrams {
  // Test用Listの定義
  val ex1: List[Int] = List.apply(1,3,5,7,9)
  val ex2: List[Int] = List.apply(2,3,10,26,1)

  // preExercise3.24: takeのテスト
  "take関数" should "Listの最初のn個からなるListを返す" in {
    assert(List.take(ex1,2) === List.apply(1,3))
    assert(List.take(ex1,3) === List.apply(1,3,5))
    assert(List.take(ex1,5) === List.apply(1,3,5,7,9))
    assert(List.take(ex2,1) === List.apply(2))
    assert(List.take(ex2,4) === List.apply(2,3,10,26))
  }

}