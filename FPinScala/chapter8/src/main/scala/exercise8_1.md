### Exercise 8.1
sum: List[Int] => Int 関数の実装を指定するプロパティを考えだせ。  
実行可能なScalaCheckコードとして記述する必要はなく、便宜的な説明でも良い。

### Answer 8.1
- リストをリバースして合計した値は、元のリストの合計した値と等しい
- リストの全ての要素が同じ値なら、リストの合計値は要素数*値と等しくなるはずである
- リストの合計値は、そのリストのheadとtailを足した値に等しい
- 空のリストの合計値は0と定義する
- リストの合計値は、要素を足す順番を変えてもその合計値は変わらない  

etc ...