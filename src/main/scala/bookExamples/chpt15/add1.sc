import scala.language.higherKinds
import bookExamples.chpt15.{Add, Reduce1}
import bookExamples.chpt15.Add._
import bookExamples.chpt15.Reduce1._

def sum[T : Add, M[_] : Reduce1](container: M[T]): T =
    implicitly[Reduce1[M]].reduce(container)(implicitly[Add[T]].add(_,_))

sum(Vector(1 -> 10, 2 -> 20, 3 -> 30))                     // Result: (6,60)
sum(1 to 10)                                               // Result: 55
sum(Option(2))                                             // Result: 2
sum[Int,Option](None)                                      // <4> ERROR!
