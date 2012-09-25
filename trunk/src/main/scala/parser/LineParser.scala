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

package gausel.parser

import  scala.util.parsing.combinator.RegexParsers

import  gausel.data._

/** Parses a line of the input syntax.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object LineParser extends RegexParsers {

  /** Same as parseLine.
   * @param l the line to parse
   * @return the corresponding arithmetic expression.
   */
  def apply(l: String) = parseLine(l)

  /** Parses a matrix / vector line.
   * @param l the line to parse
   * @return the corresponding arithmetic expression.
   */
  def parseLine(l: String): List[Option[Arith]] = parse(line,l) match {
    case Success(res,_) => res
    case Failure(msg,in) =>
      throw new ParsingException(msg,in.pos.longString)
    case Error(msg,in) =>
      throw new ParsingException(msg,in.pos.longString)
  }

  /** Parser for a matrix or vector line. */
  def line: Parser[List[Option[Arith]]] = rep1(someArith) ^^ { case ariths => ariths.toList }

  /** Parser for option of arith. */
  def someArith: Parser[Option[Arith]] = (
    zero ^^ { case none => none }
    | arith ^^ { case a => Some(a) }
  )

  /** Parser for arithmetic expressions. */
  def arith: Parser[Arith] =
    ident | integerAsBigDecimal | bigDecimal | sum | subn | sub | mult | div

  /** Parser for Zero. */
  def zero: Parser[Option[Arith]] = "0" ^^ { case _ => None }

  /** Identifier parser. */
  def ident: Parser[Arith] =
    """[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ { case id => Ident(id) }

  /** Parser for integer values.
   * Does not parse 0 as it is a special value.
   */
  def integerAsBigDecimal: Parser[Arith] =
    """([1-9][0-9]*)""".r ^^ { case s => Value(BigDecimal(s)) }

  /** Parser for decimal values. */
  def bigDecimal: Parser[Arith] =
    """(0|([1-9][0-9]*))\.([0-9]*)?""".r ^^ { case s => Value(BigDecimal(s)) }

  /** Parser for unary minus. */
  def uminus: Parser[Arith] =
    "-" ~> arith ^^ { case a => UMinus(a) }

  /** Parser for sum. */
  def sum: Parser[Arith] =
    "(" ~> arith ~ "+" ~ arith <~ ")" ^^ { case l~_~r => Plus(l,r) }

  /** Parser for the string "-". */
  def minus: Parser[String] = "-"

  /** Parser for n-ary substraction. */
  def subn: Parser[Arith] =
    "(" ~> arith ~ minus ~ arith ~ minus ~ rep1sep(arith,minus) <~ ")" ^^ {
      case k1~_~k2~_~ks => Minus1N(k1,k2::(ks.toList))
    }

  /** Parser for substraction. */
  def sub: Parser[Arith] =
    "(" ~> arith ~ minus ~ arith <~ ")" ^^ { case l~_~r => Minus(l,r) }

  /** Parser for multiplication. */
  def mult: Parser[Arith] =
    "(" ~> arith ~ "*" ~ arith <~ ")" ^^ { case l~_~r => Mult(l,r) }

  /** Parser for division. */
  def div: Parser[Arith] =
    "(" ~> arith ~ "/" ~ arith <~ ")" ^^ { case l~_~r => Div(l,r) }

}
