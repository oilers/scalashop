def scanLeft[A](input: Array[A], a0: A, f: (A,A)=>A, out: Array[A]): Unit = {
  var i = 0
  var a = a0
  out(i) = a
  while( i < input.length){
    a = f(a, input(i))
    out(i + 1) = a
    i = i+1
  }
}

var input = Array(1, 2, 3)
var output = Array(1, 2, 3, 4)
scanLeft[Int](input, 100, (x, y) => x + y, output)
output mkString ", "
