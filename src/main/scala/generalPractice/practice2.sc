
import scala.util.{Try, Success, Failure}
//Common operations on Lists

List(1, 2, 3).map(x => x + 1) == List(2, 3, 4)

List(1, 2, 3).filter(x => x % 2 == 0) == List(2)

/*Transform each element of a list into a list and flatten
the results into a single list using flatMap*/
val xs =
  List(1, 2, 3).flatMap { x =>
    List(x, 2 * x, 3 * x)
  }
xs == List(1, 2, 3, 2, 4, 6, 3, 6, 9)

//OPTIONAL VALUES

/*We represent an optional value of type A with the type Option[A].
This is useful to implement partially defined functions*/


/*An Option[A] can either be None (if there is no value)
 or Some[A] (if there is a value):*/
def sqrt(x: Double): Option[Double] =
  if (x < 0) None else Some(x * x)

//Manipulate Options with pattern matching

def foo(x: Double): String =
  sqrt(x) match {
    case None => "no result"
    case Some(y) => y.toString
  }

//Map
Some(1).map(x => x + 1) == Some(2)
None.map((x: Int) => x + 1) == None
Some(-1).map(x => x + 1) == Some(0)

//Filter
Some(1).filter(x => x % 2 == 0) == None
Some(2).filter(x => x % 2 == 0) == Some(2)

Some(8).filter(x => x % 2 == 0) == Some(8)


//Error Handling
def sqrt(x: Double): Try[Double] =
  if (x < 0) Failure(new IllegalArgumentException("x must be positive")) else Success( x * x )

def sqrt(x: Double): Either[String, Double] =
  if (x < 0){
    Left("x must be positive")
  } else Right( x * x )


/*Either also has a filterOrElse method that turns a Right value
into a Left value if it does not satisfy a given predicate */
Right(1).filterOrElse(x => x % 2 == 0, "Odd value") == Left("Odd value")


def triple(x: Int): Int = 3 * x

def tripleEither(x: Either[String, Int]): Either[String, Int] =
  x.right.map(triple)

tripleEither(Right(1)) == Right(3)

tripleEither(Left("not a number")) == Left("not a number")
