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

/** Simple verbose trait providing printing modulo an integer.
 * Prefixes the print with the name defined.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
trait Verb {
  val verbLevel: Int
  val name: String

  def verb(s: => String, v: Int = 1) = if (v <= verbLevel) print("[\033[32;1m" + name + "\033[0m] " + s)
  def verbln(s: => String, v: Int = 1) = if (v <= verbLevel) println("[\033[32;1m" + name + "\033[0m] " + s)
  def verbln(v: Int) = if (v <= verbLevel) println
  def verbList(ss: => List[String], v: Int = 1) =
    if (v <= verbLevel) for (s <- ss) println("[\033[32;1m" + name + "\033[0m] " + s)
}
