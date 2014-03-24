package sort

import scala.annotation.tailrec

/** Contains all the sorting algorithms defined by Uchuu. */
package object uchuu {

  /** Sorts by Uchuu. */
  trait UchuuSort extends Sort { val author = "uchuu" }
  /** Sorts by Dumb. */
  trait DumbSort extends Sort { val author = "dumb" }

  /** Quick sort object. */
  val quickSort = new UchuuSort {

    val name = "quick sort"

    def apply(toSort: List[Element], timeout: Int) = {
      def loop(
        list: List[Element], continuation: List[Element] => List[Element] = l => l
      ): List[Element] = list match {
        case _ :: Nil | Nil => continuation(list)
        case pivot :: t => {
          val (le,gt) = split(t, e => e <= pivot)
          Timer.check
          loop(le, sortedLe => loop(gt, sortedGt => continuation(sortedLe ++ (pivot :: sortedGt))))
        }
      }
      Timer.start(timeout)
      val res = loop(toSort)
      res
    }
    @tailrec
    def split(
      list: List[Element], pred: Element => Boolean, isTrue: List[Element] = Nil, isFalse: List[Element] = Nil
    ): (List[Element],List[Element]) = list match {
      case h :: t if pred(h) => split(t, pred, h :: isTrue, isFalse)
      case h :: t => split(t, pred, isTrue, h :: isFalse)
      case Nil => (isTrue,isFalse)
    }
  }

  /** Insertion sort object. */
  val insertionSort = new UchuuSort {

    val name = "insertion sort"

    def apply(toSort: List[Element], timeout: Int) = {
      @tailrec
      def loop(list: List[Element], sortedList: List[Element] = Nil): List[Element] = list match {
        case h :: t => { Timer.check ; loop(t, insert(h, sortedList)) }
        case Nil => sortedList
      }
      Timer.start(timeout)
      loop(toSort)
    }

    /** Inserts an element into a sorted list.
      * @param element The element to insert.
      * @param list A sorted list in which element will be inserted.
      * @param revPrefix For recursion, contains the elements of the list
      * visited so far in reverse order (default '''Nil'''). */
    @tailrec
    private def insert(
      element: Element, list: List[Element], revPrefix: List[Element] = Nil
    ): List[Element] = list match {
      case h :: t =>
        if (element <= h) revPrefix reverse_::: (element :: h :: t)
        else insert(element, t, h :: revPrefix)
      case Nil => (element :: revPrefix).reverse
    }
  }

  /** Merge sort object. */
  val mergeSort = new UchuuSort {
    val name = "merge sort"
    def apply(list: List[Element], timeout: Int) = {
      @tailrec
      def bottomUpMerge(
        list: List[List[Element]], temp: List[List[Element]] = Nil
      ): List[Element] = { Timer.check ; list match {
        case h1 :: h2 :: t => bottomUpMerge(t, sublistSorting(h1,h2) :: temp)
        case h :: Nil => if (temp.size == 0) h else bottomUpMerge(h :: temp)
        case Nil => temp match {
          case h :: Nil => h
          case _ => bottomUpMerge(temp)
        }
      }}

      Timer.start(timeout)
      bottomUpMerge(list map (e => e :: Nil))
    }

    @tailrec
    def sublistSorting(
      sList1: List[Element], sList2: List[Element], revPrefix: List[Element] = Nil
    ): List[Element] = (sList1,sList2) match {
      case (h1 :: t1, h2 :: t2) if (h2 <= h1) => sublistSorting(sList1, t2, h2 :: revPrefix)
      case (h1 :: t1, h2 :: t2) if (h1 <= h2) => sublistSorting(t1, sList2, h1 :: revPrefix)
      case (_, Nil) => revPrefix reverse_::: sList1
      case (Nil, _) => revPrefix reverse_::: sList2
      case _ => throw new Exception("Should be unreachable.")
    }
  }

  /** Bubble sort object. */
  val bubbleSort = new UchuuSort {
    val name = "bubble sort"

    def apply(list: List[Element], timeout: Int) = {
      @tailrec
      def loop(l: List[Element]): List[Element] = {
        Timer.check
        val (swappedList,swapped) = swap(l)
        if (swapped) loop(swappedList) else swappedList
      }
      Timer.start(timeout)
      loop(list)
    }

    @tailrec
    def swap(
      list: List[Element], revPrefix: List[Element] = Nil, swapped: Boolean = false
    ): (List[Element], Boolean) = list match {
      case h1 :: h2 :: t if h2 < h1 => swap(h1 :: t, h2 :: revPrefix, true)
      case h1 :: h2 :: t if h1 <= h2 => swap(h2 :: t, h1 :: revPrefix, swapped)
      case h :: Nil => ((h :: revPrefix).reverse, swapped)
      case Nil => (revPrefix.reverse, swapped)
    }
  }

}
