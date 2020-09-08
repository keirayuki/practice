import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_28 extends AnyFlatSpec with Diagrams {
  // Test用Branchの定義
  val ex1: Tree[Int] = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(10),Leaf(29)))
  val ex2: Tree[Int] = Branch(Leaf(20),Branch(Leaf(10),Leaf(5)))

  // Exercise3.28: mapのテスト
  // 今回は１を加える関数
  "map関数" should "２分木の各要素を関数を使って変更する。" in {
    assert(Tree.map(ex1)(x => x+1) === Branch(Branch(Leaf(2),Leaf(3)),Branch(Leaf(11),Leaf(30))))
    assert(Tree.map(ex2)(x => x+1) === Branch(Leaf(21),Branch(Leaf(11),Leaf(6))))
  }

}