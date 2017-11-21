package listimplimentation
import scala.annotation.tailrec

sealed trait List[+A] {
  override def toString = {
    def toScalaList(t: List[A]): scala.List[A] = t match {
      case Empty => Nil
      case Cons(h, t) => h :: toScalaList(t)
    }
    toScalaList(this).toString
  }
}
final case object Empty extends List[Nothing]
final case class Cons[A](h: A, t: List[A]) extends List[A]
