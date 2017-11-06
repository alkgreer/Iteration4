import scala.language.higherKinds
import bookExamples.chpt15.{Add, Reduce}    // <1>
import bookExamples.chpt15.Add._
import bookExamples.chpt15.Reduce._

def sum[T : Add, M[T]](container: M[T])(                   // <2>
  implicit red: Reduce[T,M]): T =
    red.reduce(container)(implicitly[Add[T]].add(_,_))

sum(Vector(1 -> 10, 2 -> 20, 3 -> 30))                     // Result: (6,60)
sum(1 to 10)                                               // Result: 55
sum(Option(2))                                             // Result: 2
sum[Int,Option](None)                                      // <3> ERROR!
