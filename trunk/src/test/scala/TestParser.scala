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

package gausel.test

/** Tests the parser.
 *
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object TestParser extends App with gausel.lib.Verb {

  // Verbose related stuff.
  val name = "TestParser"
  val verbLevel = 1
  val color = Colors.cyan

  val toParse =
    "title {"::
    "  One two one two it's just a test."::
    "}"::
    ""::
    "matrix {"::
    "  0 a12 a13"::
    "  a21 a22 0"::
    "  0 a32 a33"::
    "}"::
    ""::
    "vector {"::
    "  b1"::
    "  b2"::
    "  b3"::
    "}"::
    ""::
    "Bla bla bla"::
    "Toto tata titi tutu"::
    "sntahoestnud;ad,.tnd"::
    ""::
    ";s,.ntphaoearicd"::
    Nil

  verbln(1)
  verbln("Parsing the following lines:")
  toParse.foreach(l => verbln("  " + l))
  val (title,matrix,vector,rest) =
    gausel.parser.SystemParser(toParse)
  verbln(1)
  verbln("Result:")
  verbln("  title:")
  verbln("    " + title)
  verbln("  matrix:")
  matrix.foreach(l => verbln("    " + l))
  verbln("  vector:")
  vector.foreach(l => verbln("    " + l))
  verbln("  rest:")
  rest.foreach(l => verbln("    " + l))
  verbln(1)

  verbln("That's all.")
  verbln("See ya.")

}
