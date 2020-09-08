import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_25 extends AnyFlatSpec with Diagrams {
  // Test用Branchの定義
  val ex1: Tree[Int] = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(10),Leaf(29)))
  val ex2: Tree[Int] = Branch(Leaf(20),Leaf(10))

  // Exercise3.25: sizeのテスト
  "size関数" should "2分木のノードの数を数える" in {
    assert(Tree.size(ex1) === 7)
    assert(Tree.size(ex2) === 3)
  }

}