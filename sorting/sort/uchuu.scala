package sort

import scala.annotation.tailrec

/** Contains all the sorting algorithms defined by Uchuu. */
package object uchuu {

  /** Sorts by Uchuu. */
  trait UchuuSort extends Sort { val author = "uchuu" }
  /** Sorts by Dumb. */
  trait DumbSort extends Sort { val author = "dumb" }

  /** Insertion sort object. */
  val insertionSort = new DumbSort {

    val name = "insertion sort"

    def apply(toSort: List[Element]) = {
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
    private def insert(
      element: Element, list: List[Element], revPrefix: List[Element] = Nil
    ): List[Element] = list match {
      case h :: t =>
        if (element <= h) revPrefix reverse_::: (element :: list)
        else insert(element, t, h :: revPrefix)
      case Nil => (element :: revPrefix).reverse
    }
  }

  /** Insertion sort object. */
  val insertionSortTailrec = new UchuuSort {

    val name = "insertion sort (tailrec)"

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
