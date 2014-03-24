import scala.annotation.tailrec

/** Contains all the interfaces and default algorithms. */
package object sort {

  /** The type of the elements of the lists to sort. */
  type Element = Int

  /** Trait the sorting objects should extend. */
  trait Sort {
    /** The name of the sorting algorithm. */
    val name: String
    /** The author of the sorting algorithm. */
    val author: String

    override def toString = name + " by " + author

    /** A timer object used to check for timeouts. */
    object Timer {
      private var startTime: Long = 0
      private var timeout: Int = 0
      def start(to: Int) = {
        timeout = to ; startTime = System.currentTimeMillis
      }
      def getTime = System.currentTimeMillis - startTime
      def check = if (timeout > 0)
        if (getTime >= timeout) throw TimeoutException(timeout) else ()
    }


    /** Sorts the input list.
      * @param toSort The list to sort.
      * @param timeout A timeout in milliseconds. */
    def apply(toSort: List[Element], timeout: Int): List[Element]
  }

  /** Checks whether a list is sorted or not.
    * @param list The list to check. */
  def isSorted(list: List[Element], original: List[Element]) = {
    @tailrec
    def loop(l: List[Element], lastElement: Element): Boolean = l match {
      case h :: t => if (lastElement <= h) loop(t,h) else false
      case Nil => true
    }
    list match {
      case Nil => original.isEmpty
      case h :: t => (list.size == original.size) && loop(t,h)
    }
  }

  /** Exception thrown when a sorting algorithm times out. */
  case class TimeoutException(val timeout: Int) extends Exception("Timeout: " + timeout + ".")
}
