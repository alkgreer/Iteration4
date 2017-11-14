// src/main/scala/Iteration/99ScalaProbs.scala

object ScalaProblems {
  def main(args: Array[String]) {

    val numList = List(0, 1, 2, 3, 4, 5, 6, 7, 8)
    val palindromeList = List(4, 1, 7, 1, 4)
    val nestedList = List(List(1, 2, 3), 4, List(5, List(6, 7)))
    val duplicates = List(1, 1, 1, 2, 2, 3, 3, 4, 5)
    val charList = List('k', 'a', 'c')

    // P01 - Find the last element of a list.
    def last[A](l: List[A]): A = l match {
      case Nil    => throw new NoSuchElementException
      case headElem::Nil => headElem
      case headElem::tailList   => last(tailList)
    }
    println(s"P01: ${last(numList)}")

    // P02 - Find the last but one element of a list.
    def penultimate[A](l: List[A]): A = l match {
      case Nil    => throw new NoSuchElementException
      case headElem::Nil => throw new NoSuchElementException
      case headElem::tailList   => tailList match {
        case headElem2::Nil => headElem
        case headElem2::tailList2  => penultimate(tailList)
      }
    }
    println(s"P02: ${penultimate(numList)}")

    // P03 - Find the Kth element of a list.
    def nth[A](n: Int, l: List[A]): A = n match {
      case 0 => l.head
      case _ => nth((n-1), l.tail)
    }
    println(s"P03: ${nth(3, numList)}")

    // P04 - Find the number of elements of a list.
    def length[A](l: List[A]): Int = l match {
      case Nil  => 0
      case headElem::tailList => 1 + length(tailList)
    }
    println(s"P04: ${length(numList)}")

    //P05 - Reverse a list.
    def reverse[A](l: List[A]): List[A] = l match {
      case headElem::tailList => reverse(tailList) ::: List(headElem)
      case Nil  => Nil
    }
    println(s"P05: ${reverse(numList)}")

    // P06 - Find out whether a list is a palindrome.
    def isPalindrome[A](l: List[A]): Boolean = l == reverse(l)
    println(s"P06: ${isPalindrome(palindromeList)}")

    // P07 - Flatten a nested list structure.
    def flatten(l: List[Any]): List[Any] = l match {
      case Nil  => Nil
      case headElem::tailList => headElem match {
        case headElem: List[Any] => flatten(headElem) ::: flatten(tailList)
        case headElem: Any       => headElem :: flatten(tailList)
      }
    }
    println(s"P07: ${flatten(nestedList)}")

    // P08 - Eliminate consecutive duplicates of list elements.
    def compress[A](l: List[A]): List[A] = l match {
      case Nil    => Nil
      case headElem::Nil => List(headElem)
      case headElem::tailList if (headElem == tailList.head) => compress(tailList)
      case headElem::tailList   => headElem :: compress(tailList)
    }
    println(s"P08: ${compress(duplicates)}")

  }
}
