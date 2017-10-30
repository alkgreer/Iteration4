"bar".take(2)
"foo".drop(1)
"foo".drop(2)
"foo".drop(3)
"foo".drop(0)
"foo".drop(3)
"foo".drop(2)
"abcde".drop(0)
"abcde".drop(1)
"abcde".drop(2)
"abcde".drop(3)
"abcde".drop(4)
"abcde".drop(5)
"abcde".take(0)
"abcde".take(1)
"abcde".take(2)
"abcde".take(3)
"abcde".take(4)
"abcde".take(5)
16.toHexString
def square(x: Double) = x * x
def area(radius: Double): Double = 3.14159 * square(radius)
area(10)
3
def triangleArea(base: Double, height: Double): Double =
base * height / 3
triangleArea(5, 6)
val x = 0
def f(y: Int) = y + 1
val result = {
  val x = f(3)
  x * x
} + x
val x  = 11
x + 2
x
def unexhaustive(): Unit = {
  sealed trait Symbol
  case class Note(name: String, duration: String, octave: Int) extends Symbol
  case class Rest(duration: String) extends Symbol

  def nonExhaustiveDuration(symbol: Symbol): String =
    symbol match {
      case Rest(duration) => duration
    }
}
def unexhaustive(): Unit = {
  sealed trait Symbol
  case class Note(name: String, duration: String, octave: Int) extends Symbol
  case class Rest(duration: String) extends Symbol

  def nonExhaustiveDuration(symbol: Symbol): String =
    symbol match {
      case Rest(duration) => duration
	  case Note(name, duration, octave) => duration
    }
}
def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(x: Int, acc: Int): Int = {
    if (x > b) acc
    else loop(x + 
1
, acc + f(x))
  }
  loop(a, 
a
)
}
sum(x => x, 1, 10)
def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(x: Int, acc: Int): Int = {
    if (x > b) acc
    else loop(x + 
1
, acc + f(x))
  }
  loop(a, 
0
)
}
sum(x => x, 1, 10)
val cond: (Int, Int) => Boolean = 
_ < _
def insert(x: Int, xs: List[Int]): List[Int] =
  xs match {
    case List() => x :: 
Nil

    case y :: ys =>
      if (cond(x, y)) x :: y :: ys
      else y :: insert(x, ys)
  }
val cond: (Int, Int) => Boolean = 
_ < _
def insert(x: Int, xs: List[Int]): List[Int] =
  xs match {
    case List() => x :: 
Nil

    case y :: ys =>
      if (cond(x, y)) x :: y :: ys
      else y :: insert(x, ys)
  }
insert(2, 1 :: 3 :: Nil)
insert(1, 2 :: 3 :: Nil)
insert(3, 1 :: 2 :: Nil)
