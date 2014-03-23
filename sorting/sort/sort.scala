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

    /** Sorts the input list.
      * @param toSort The list to sort. */
    def apply(toSort: List[Element]): List[Element]
  }

  /** Checks whether a list is sorted or not.
    * @param list The list to check. */
  def isSorted(list: List[Element]) = {
    @tailrec
    def loop(l: List[Element], lastElement: Element): Boolean = l match {
      case h :: t => if (lastElement <= h) loop(t,h) else false
      case Nil => true
    }
    list match {
      case Nil => true
      case h :: t => loop(t,h)
    }
  }

}
