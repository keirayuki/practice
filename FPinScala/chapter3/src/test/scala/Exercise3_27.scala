import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_27 extends AnyFlatSpec with Diagrams {
  // Test用Branchの定義
  val ex1: Tree[Int] = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(10),Leaf(29)))
  val ex2: Tree[Int] = Branch(Leaf(20),Branch(Leaf(10),Leaf(5)))
  val ex3: Tree[Int] = Branch(Branch(Branch(Leaf(10),Leaf(20)),Leaf(10)),Branch(Leaf(10),Leaf(10)))

  // Exercise3.27: depthのテスト
  "depth関数" should "ルートから任意のリーフまでの最長パスを返す" in {
    assert(Tree.depth(ex1) === 2)
    assert(Tree.depth(ex2) === 2)
    assert(Tree.depth(ex3) === 3)
  }

}