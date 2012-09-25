/*
 * Copyright (c) 2012 Dame Ningen.
 * All rights reserved.
 *
 * This file is part of Gausel.
 *
 * Gausel is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Gausel is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Gausel.  If not, see <http://www.gnu.org/licenses/>.
 */

package gausel.lib

import scala.annotation.tailrec

/** Trait for sorting algorithms.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
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

/** Insertion sort.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
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
