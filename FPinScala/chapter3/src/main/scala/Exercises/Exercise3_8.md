### Exercise3.8
foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))のように、  
NilおよびCons自体をfoldRightに渡した場合はどうなるか。


### 3.8 Answer
Cons(1,Cons(2,Cons(3,Nil)))が返る。