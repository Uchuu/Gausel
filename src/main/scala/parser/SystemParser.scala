package gausel.parser

import  scala.util.parsing.combinator.RegexParsers

import  gausel.data._

object SystemParser extends RegexParsers {

  /** Extracts the title, matrix and vector from a list of Strings.
   * @param ls the lines to extract the system from
   * @return the title, matrix and vector and the rest of the input
   */
  def apply(ls: List[String]) = {
    val lines = ls.filterNot(_ == "")
    val (title,next1) = parseTitle(lines)
    val (matrix,next2) = parseMatrix(next1)
    val (vector,next3) = parseVector(next2)
    val rest = next3 match {
      case Nil => Nil
      case _ => ls.dropWhile(_ != next3.head)
    }
    (title,matrix,vector,rest)
  }

  /** Parses a title.
   * @param lines the lines to parse
   * @return the title and the remaining lines
   */
  def parseTitle(lines: List[String]): (String,List[String]) = lines match {
    case h::t => parse(beginTitleDef,h) match {
      case m@Success(res,_) => {
        def loop(ls: List[String],
                 res: String = ""): (String,List[String]) = ls match {
          case h::t => parse(endDef,h) match {
            case Success(_,_) => (res,t)
            case _ => parse(something,h) match {
              case Success(title,_) => loop(t,if (res=="") title.dropWhile(_ == ' ')
                                              else res + " " + title.dropWhile(_ == ' '))
              case Failure(msg,in) =>
                throw new ParsingException(msg,in.pos.longString)
              case Error(msg,in) =>
                throw new ParsingException(msg,in.pos.longString)
            }
          }
          case Nil =>
            throw new ParsingException("Premature end of file while parsing title.","")
        }
        loop(t)
      }
      case Failure(msg,in) =>
        throw new ParsingException(msg,in.pos.longString)
      case Error(msg,in) =>
        throw new ParsingException(msg,in.pos.longString)
    }
    case Nil =>
      throw new ParsingException("Premature end of file while parsing title.","")
  }

  /** Parses a matrix.
   * @param lines the lines to parse
   * @return the matrix and the remaining lines
   */
  def parseMatrix(lines: List[String]): (List[List[Option[Arith]]],List[String]) = lines match {
    case h::t => parse(beginMatrixDef,h) match {
      case m@Success(res,_) => {
        // Note that res is in reverse order.
        def loop(ls: List[String],
                 res: List[List[Option[Arith]]] = Nil): (List[List[Option[Arith]]],List[String]) = ls match {
          case h::t => parse(endDef,h) match {
            case Success(_,_) => (res.reverse,t)
            case _ => loop(t,LineParser(h)::res)
          }
          case Nil =>
            throw new ParsingException("Premature end of file while parsing matrix.","")
        }
        loop(t)
      }
      case Failure(msg,in) =>
        throw new ParsingException(msg,in.pos.longString)
      case Error(msg,in) =>
        throw new ParsingException(msg,in.pos.longString)
    }
    case Nil =>
      throw new ParsingException("Premature end of file while parsing matrix.","")
  }

  /** Parses a vector.
   * @param lines the lines to parse
   * @return the vector and the remaining lines
   */
  def parseVector(lines: List[String]): (List[Option[Arith]],List[String]) = lines match {
    case h::t => parse(beginVectorDef,h) match {
      case m@Success(res,_) => {
        // Note that res is in reverse order.
        def loop(ls: List[String],
                 res: List[Option[Arith]] = Nil): (List[Option[Arith]],List[String]) = ls match {
          case h::t => parse(endDef,h) match {
            case Success(_,_) => (res.reverse,t)
            case _ => LineParser(h) match {
              case value::Nil => loop(t,value::res)
              case value::_ =>
                throw new ParsingException("More than one value in this vector line.",h)
              case Nil => throw new Exception("Weird")
            }
          }
          case Nil =>
            throw new ParsingException("Premature end of file while parsing vector.","")
        }
        loop(t)
      }
      case Failure(msg,in) =>
        throw new ParsingException(msg,in.pos.longString)
      case Error(msg,in) =>
        throw new ParsingException(msg,in.pos.longString)
    }
    case Nil =>
      throw new ParsingException("Premature end of file while parsing vector.","")
  }

  /** Parser for the beginning of a title definition. */
  def beginTitleDef: Parser[Unit] = "title" ~ "{" ^^ {case _~_ => ()}

  /** Parser for the beginning of a matrix definition. */
  def beginMatrixDef: Parser[Unit] = "matrix" ~ "{" ^^ {case _~_ => ()}

  /** Parser for the beginning of a vector definition. */
  def beginVectorDef: Parser[Unit] = "vector" ~ "{" ^^ {case _~_ => ()}

  /** Parser for the end of a definition. */
  def endDef: Parser[Unit] = "}" ^^ {case _ => ()}

  /** Parser for anything but curly braces. */
  def something: Parser[String] = """[^{}]+""".r

}

class ParsingException(val error: String,
                       val input: String) extends Exception("Parsing error: " + error + ".")

