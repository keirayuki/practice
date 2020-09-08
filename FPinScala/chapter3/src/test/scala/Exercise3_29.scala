import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import datastructures._


class Exercise3_29 extends AnyFlatSpec with Diagrams {
  // Test用Branchの定義
  val ex1: Tree[Int] = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(10),Leaf(29)))
  val ex2: Tree[Int] = Branch(Leaf(20),Branch(Leaf(10),Leaf(5)))

  // Exercise3.29: foldのテスト
  // size2のテスト
  "size2関数" should "2分木のノードの数を数える" in {
    assert(Tree.size2(ex1) === 7)
    assert(Tree.size2(ex2) === 5)
  }
  // maximum2のテスト
  "maximum2関数" should "2分木のノードの最大要素を返す" in {
    assert(Tree.maximum2(ex1) === 29)
    assert(Tree.maximum2(ex2) === 20)
  }
  // depth2関数のテスト
  "depth2関数" should "ルートから任意のリーフまでの最長パスを返す" in {
    assert(Tree.depth2(ex1) === 2)
    assert(Tree.depth2(ex2) === 2)
  }
  // map2関数のテスト
  // 今回は各要素に＋１する
  "map2関数" should "２分木の各要素を関数を使って変更する。" in {
    assert(Tree.map2(ex1)(x => x+1) === Branch(Branch(Leaf(2),Leaf(3)),Branch(Leaf(11),Leaf(30))))
    assert(Tree.map2(ex2)(x => x+1) === Branch(Leaf(21),Branch(Leaf(11),Leaf(6))))
  }

}