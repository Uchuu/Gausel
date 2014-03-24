package sorttester

import scala.annotation.tailrec

import sort._

/** Package object for an author called Foufou.
  * Sort objects defined in this package should be registered in
  * '''Run.scala'''.
  */
package object foufou {

  /** A fake sort that does nothing. */
  val fakeSort = new Sort {

    // The author of the sort.
    val author = "foufou"
    // The name of the sorting algorithm.
    val name = "fake sort"

    /** This function is automatically called by Run.scala. */
    def apply(toSort: List[Element], timeout: Int) = {
      // Users need to check for timeout themselves. First, start the Timer.
      Timer.start(timeout)

      // Then regularly check that the timeout has not been reached
      // in the function used for sorting.
      @tailrec
      def loop(list: List[Element]): List[Element] = list match {
        // Iterates on the element of the list...
        case h :: t => {
          // ... checks each time if the timeout has been reached...
          Timer.check
          // ... iterates without doing nothing...
          loop(t)
        }
        // ... and returns the input list.
        case Nil => toSort
      }

      // Sorting the list and regularly checking for timeout.
      loop(toSort)
    }

  }

  /** A fake sort that reverts its input list. */
  val fakeRevSort = new Sort {

    // The author of the sort.
    val author = "foufou"
    // The name of the sorting algorithm.
    val name = "fake rev sort"

    def apply(toSort: List[Element], timeout: Int) = {
      def loop(list: List[Element], res: List[Element]): List[Element] = list match {
        case h :: t => { Timer.check ; loop(t,h :: res) }
        case Nil => res
      }
      Timer.start(timeout)
      loop(toSort,Nil)
    }

  }

}
