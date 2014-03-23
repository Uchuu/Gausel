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

    def apply(toSort: List[Element]) = {
      def loop(
        list: List[Element], continuation: List[Element] => List[Element] = l => l
      ): List[Element] = list match {
        case _ :: Nil | Nil => continuation(list)
        case pivot :: t => {
          val (le,gt) = split(t, e => e <= pivot)
          loop(le, sortedLe => loop(gt, sortedGt => sortedLe ++ (pivot :: sortedGt)))
        }
      }
      loop(toSort)
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

    def apply(toSort: List[Element]) = {
      @tailrec
      def loop(list: List[Element], sortedList: List[Element] = Nil): List[Element] = list match {
        case h :: t => loop(t, insert(h, sortedList))
        case Nil => sortedList
      }
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

  /** Bubble sort object. */
  val bubbleSort = new UchuuSort {
    val name = "bubble sort"
    def apply(list: List[Element]) = list
  }

}
