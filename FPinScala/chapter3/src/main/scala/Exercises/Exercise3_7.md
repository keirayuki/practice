### Exercise3.7
foldRightを使って実装されたproductは、0.0を検出した場合に、  
直ちに再帰を中止して0.0を返せるか。その理由を説明せよ。

### 3.7 Answer
不可。  
foldRightは、先にリストの末尾まで展開し、その後に畳み込みを  
開始するため、途中で0.0を検出したとしても、次のリストの展開が行われる。

```scala
def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
        case Nil => z
        case Cons(x,xs) => f(x, foldRight(xs,z)(f))
    }
```
