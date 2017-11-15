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

    // P09 - Pack consecutive duplicates of list elements into sublists.
    def pack[A](l: List[A]): List[List[A]] = {
      def _pack(groups: List[List[A]], l: List[A]): List[List[A]] = l match {
        case Nil  => groups
        case headElem::tailList if (groups.isEmpty || groups.last.head != headElem) => _pack(groups:::List(List(headElem)), tailList)
        case headElem::tailList => _pack(groups.init:::List(groups.last:::List(headElem)), tailList)
      }
      _pack(Nil, l)
    }
    println(s"P09: ${pack(duplicates)}")

    // P10 - Run-length encoding of a list.
    def encode[A](l: List[A]): List[(Int,A)] = {
      val packed = pack(l)
      for (sym <- packed) yield (length(sym), sym.head)
    }
    println(s"P10: ${encode(duplicates)}")

    // P12 - Decode a run-length encoded list.
    def decode[A](l: List[(Int,A)]): List[Any] = {
      def _expand(n: Int, a: A, group: List[A]): List[A] = n match {
        case 0 => group
        case _ => _expand((n-1), a, group:::List(a))
      }
      val groups = for (sym <- l) yield _expand(sym._1, sym._2, Nil)
      flatten(groups)
    }
    println(s"P12: ${decode(encode(duplicates))}")

    // P13 - Run-length encoding of a list (direct solution).
    def encodeDirect[A](l: List[A]): List[(Int,A)] = {
      def _encode(groups: List[(Int,A)], l: List[A]): List[(Int,A)] = l match {
        case Nil => groups
        case headElem::tailList if (groups.isEmpty || groups.last._2 != headElem) => _encode(groups:::List((1,headElem)), tailList)
        case headElem::tailList => _encode(groups.init:::List(((groups.last._1 + 1),headElem)), tailList)
      }
      _encode(Nil, l)
    }
    println(s"P10: ${encodeDirect(duplicates)}")

    // P14 - Duplicate the elements of a list.
    def duplicate[A](l: List[A]): List[Any] = {
      flatten(for (sym <- l) yield List(sym, sym))
    }
    println(s"P14: ${duplicate(charList)}")

    // P15 - Duplicate the elements of a list a given number of times.
    def duplicateN[A](n: Int, l: List[A]): List[Any] = {
      def _addN(dups: List[A], n: Int, sym: A): List[A] = n match {
        case 0 => dups
        case _ => _addN(sym::dups, (n-1), sym)
      }
      flatten(for (sym <- l) yield _addN(Nil, n, sym))
    }
    println(s"P15: ${duplicateN(4, charList)}")

    // P16 - Drop every Nth element from a list.
    def drop[A](n: Int, l: List[A]): List[A] = {
      def _drop(n: Int, tl: List[A]): List[A] = n match {
        case 0 => tl.tail
        case _ => tl.head :: _drop((n-1), tl.tail)
      }
      _drop(n, l)
    }
    println(s"P16: ${drop(1, charList)}")

  }
}
