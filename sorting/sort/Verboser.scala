package sort

import scala.annotation.tailrec

/** Provides verbose primitives, with a colored prefix. */
trait Verboser {

  /** A name to prefix the prints. If none is given there will be no prefix. */
  protected val name: Option[String]
  /** The color used for the prefix. Predefined colors are given in the '''Color''' object. */
  protected val color: Color
  /** The verbose level of this instance. */
  protected var verbLevel: Int
  /** The prefix to use when printing something. */
  lazy protected val prefix = name match {
    case Some(s) => "[\033[" + color + ";1m" + s + "\033[0m] "
    case None => ""
  }

  /** Prints the input string if the level specified is less than the
    * '''verbLevel'''.
    * @param s The string to print if '''level''' is less than '''verbLevel'''.
    * @param level The level of this print. */
  protected def verb(s: => String, level: Int = 1) = if (level <= verbLevel) print(s)
  /** Prints the prefix and the input string if the level specified is
    * less than the '''verbLevel'''.
    * @param s The string to print if '''level''' is less than '''verbLevel'''.
    * @param level The level of this print. */
  protected def verbPrefix(s: => String, level: Int = 1) = if (level <= verbLevel) print(prefix + s)
  /** Prints the prefix, the input string and a newline if the level
    * specified is less than the '''verbLevel'''.
    * @param s The string to print if '''level''' is less than '''verbLevel'''.
    * @param level The level of this print. */
  protected def verbln(s: => String, level: Int = 1) = if (level <= verbLevel) println(prefix + s)
  /** Prints a list of strings with prefix and newline between strings
    * if '''level''' is less than the '''verbLevel'''.
    * @param ss The list of strings to print.
    * @param lever The level of this print. */
  protected def verbList(ss: => List[String], level: Int = 1) =
    if (level <= verbLevel) ss foreach (s => verbln(s,level))


  /** Gathers all the colors usable in the verboser. */
  protected sealed trait Color
  /** Red is 31. */
  protected object Red extends Color { override def toString = 31.toString }
  /** Green is 32. */
  protected object Green extends Color { override def toString = 32.toString }
  /** Yellow is 33. */
  protected object Yellow extends Color { override def toString = 33.toString }
  /** Blue is 34. */
  protected object Blue extends Color { override def toString = 34.toString }
  /** Magenta is 35. */
  protected object Magenta extends Color { override def toString = 35.toString }
  /** Cyan is 36. */
  protected object Cyan extends Color { override def toString = 36.toString }

}
