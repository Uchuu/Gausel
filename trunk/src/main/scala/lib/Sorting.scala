package gausel.lib

import scala.annotation.tailrec

trait Sorting[T] {
  /** Sorts the input list.
   * @param l the list to sort
   * @return the corresponding sorted list with respect to infEq
   */
  def sort1(l: List[T]): List[T]
  /** A total relation.
   * @param l the first element to compare
   * @param r the second element to compare
   * @return a Boolean
   */
  def infEq1(l: T, r: T): Boolean
  /** Sorts the input list.
   * @param l the list to sort
   * @return the corresponding sorted list with respect to infEq
   */
  def sort2(l: List[T]): List[T]
  /** A total relation.
   * @param l the first element to compare
   * @param r the second element to compare
   * @return a Boolean
   */
  def infEq2(l: T, r: T): Boolean
}

trait InsertionSort[T] extends Sorting[T] {

  def sort1(l: List[T]) = sort(l,infEq1)
  def sort2(l: List[T]) = sort(l,infEq2)

  def sort(l: List[T],
           infEq: (T,T) => Boolean) = {
    def loop(list: List[T] = l,
             res: List[T] = Nil): List[T] = list match {
      case h::t => {
        val newRes = insert(h,res)
        loop(t,newRes)
      }
      case Nil => res.reverse
    }
    @tailrec
    def insert(e: T,
               suffix: List[T],
               prefix: List[T] = Nil): List[T] = suffix match {
      case h::t if !infEq(h,e) => insert(e,t,h::prefix)
      // Prefix is in reverse order.
      case h::t => prefix reverse_::: e::suffix
      case Nil => (e::prefix).reverse
    }
    loop()
  }
}
