import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_26 extends AnyFlatSpec with Diagrams {
  // Test用Branchの定義
  val ex1: Tree[Int] = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(10),Leaf(29)))
  val ex2: Tree[Int] = Branch(Leaf(20),Leaf(10))

  // Exercise3.26: maximumのテスト
  "maximum関数" should "2分木のノードの最大要素を返す" in {
    assert(Tree.maximum(ex1) === 29)
    assert(Tree.maximum(ex2) === 20)
  }

}