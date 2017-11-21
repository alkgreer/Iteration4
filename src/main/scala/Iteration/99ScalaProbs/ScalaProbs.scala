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

    // P17 - Split a list into two parts.
    def split[A](n: Int, l: List[A]): (List[A], List[A]) = {
      def _split(n: Int, hd: List[A], tl: List[A]): (List[A], List[A]) = n match {
        case 0 => (hd, tl)
        case _ => _split((n-1), hd:::List(tl.head), tl.tail)
      }
      _split(n, Nil, l)
    }
    println(s"P17: ${split(4, numList)}")

    // P18 - Extract a slice from a list.
    def slice[A](nF: Int, nL: Int, l: List[A]): List[A] = {
      def _slice(i: Int, slc: List[A]): List[A] = i match {
        case x if x >= nL => slc
        case x if x >= nF => _slice((i+1), slc:::List(nth(i, l)))
        case _            => _slice((i+1), slc)
      }
      _slice(0, Nil)
    }
    println(s"P18: ${slice(3, 7, numList)}")

    // P19 - Rotate a list N places to the left.
    def rotate[A](n: Int, l: List[A]): List[A] = {
      val normN = if (n < 0) { length(l) + n } else { n }
      def _rotate(i: Int, hd: List[A], tl: List[A]): List[A] = i match {
        case x if x >= normN  => tl:::hd
        case _                => _rotate((i+1), hd:::List(tl.head), tl.tail)
      }
      _rotate(0, Nil, l)
    }
    println(s"P19: ${rotate(-2, numList)}")

    // P20 - Remove the Kth element from a list.
    def removeAt[A](n: Int, l: List[A]): (List[A], A) = {
      def _removeAt(i: Int, hd: List[A], tl: List[A]): (List[A], A) = i match {
        case x if x == n  => (hd:::tl.tail, tl.head)
        case _            => _removeAt((i+1), hd:::List(tl.head), tl.tail)
      }
      _removeAt(0, Nil, l)
    }
    println(s"P20: ${removeAt(4, numList)}")

    // P21 - Insert an element at a given position into a list.
    def insertAt[A](sym: A, n: Int, l: List[A]): List[A] = {
      def _insertAt(i: Int, hd: List[A], tl: List[A]): List[A] = i match {
        case x if x == n  => hd:::List(sym):::tl
        case _            => _insertAt((i+1), hd:::List(tl.head), tl.tail)
      }
      _insertAt(0, Nil, l)
    }
    println(s"P21: ${insertAt("new", 1, charList)}")

    // P22 - Create a list containing all integers within a given range.
    def range(nF: Int, nL: Int): List[Int] = {
      def _range(i: Int, l: List[Int]): List[Int] = i match {
        case x if x <= nL => _range((i+1), l:::List(i))
        case _            => l
      }
      _range(nF, Nil)
    }
    println(s"P22: ${range(4, 9)}")

    // P23 - Extract a given number of randomly selected elements from a list.
    def randomSelect[A](n: Int, l: List[A]): List[A] = {
      val r = new util.Random
      def _randomSelect(i: Int, rands: List[A], l: List[A]): List[A] = i match {
        case x if x == n  => rands
        case _            => {
          val rem = removeAt(r.nextInt(length(l)), l)
          _randomSelect((i+1), rands:::List(rem._2), rem._1)
        }
      }
      _randomSelect(0, Nil, l)
    }
    println(s"P23: ${randomSelect(3, numList)}")

    // P24 - Draw N different random numbers from the set 1..M.
    def lotto(n: Int, m: Int): List[Int] = {
      val r = new util.Random
      val pool = range(1, m)
      randomSelect(n, pool)
    }
    println(s"P24: ${lotto(6, 49)}")

    // P25 - Generate a random permutation of the elements of a list.
    def randomPermute[A](l: List[A]): List[A] = {
      val r = new util.Random
      def _randomPermute(oldL: List[A], newL: List[A]): List[A] = oldL match {
        case Nil  => newL
        case _    => {
          val rem = removeAt(r.nextInt(length(oldL)), oldL)
          _randomPermute(rem._1, newL:::List(rem._2))
        }
      }
      _randomPermute(l, Nil)
    }
    println(s"P25: ${randomPermute(numList)}")

  }
}
