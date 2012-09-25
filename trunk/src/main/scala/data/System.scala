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

package gausel.data

import scala.collection.mutable.OpenHashMap

/** Class representing a line.
 * Maps a column index to its coefficient, an arithmetic expression.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
abstract class Line extends OpenHashMap[Int,Arith] {

  /** The arithmetic expression the line taken as a sum is equal to. */
  val b: Arith

  /** Gets the first non zero in the line and its column index.
   * /!\ Loops forever if the line is empty /!\
   * @return a couple containing the first non zero coefficient
   *         and its column index
   */
  def getFirstCoeff() = {
    def loop(acc: Int = 1): (Arith,Int) = this get acc match {
      case Some(coef) => (coef,acc)
      case None => loop(acc+1)
    }
    loop()
  }

  /** Gets a coefficient in the line.
   * @param col the column index of the coefficient
   * @return the coefficient at the column index
   */
  def getCoeff(col: Int) = this get col

  /** Returns the number of zeros at the beginning of the line. */
  def cardFirstZeros() = {
    def loop(col: Int = 1): Int = this get col match {
      case None => loop(col+1)
      case _ => col - 1
    }
    loop()
  }

  /** Removes a coefficient from the line.
   * @param col the column index of the coefficient to remove
   */
  def removeCoeff(col: Int) = this remove col

  /** Multiplies this by a factor.
   * Called mult to be coherent with minus (see below), eventhough
   * "*" is not defined by OpenHashMap.
   * @param factor the factor to multiply this with
   * @return this multiplied by a factor
   */
  def mult(factor: Arith) = {
    val thisB = b
    val res = new Line { val b = Mult(factor,thisB) }
    // Not very efficient, would be better to use an iterator.
    this.foreach((c) => res.update(c._1,Mult(factor,c._2)))
    res
  }

  /** Substracts two lines.
   * Cannot call it "-" since OpenHashMap already defines it, and I
   * think overriding it is errorprone.
   * @param that the line which will be substracted to this.
   * @return this - that
   */
  def minus(that: Line) = {
    val thisB = b
    val maxIndex =
      math.max(this.foldLeft(0)((max,c) => math.max(max,c._1)),
               that.foldLeft(0)((max,c) => math.max(max,c._1)))
    val res = new Line { val b = Minus(thisB,that.b) }
    // Quite inelegant, but I'm too tired right now to make it better.
    // It would be more efficient to iterate on this, getting in that
    // and update res while memorizing the indices encountered.
    // Then iterate on that for the missing indices.
    // Anyways... crappy loop.
    def loop(col: Int = 1): Unit =
      if (col > maxIndex) ()
      else (this get col, that get col) match {
        case (None,None) => loop(col + 1)
        case (Some(arith1),None) => {
          res.update(col,arith1)
          loop(col+1)
        }
        case (None,Some(arith2)) => {
          res.update(col,UMinus(arith2))
          loop(col+1)
        }
        case (Some(arith1),Some(arith2)) => {
          res.update(col,Minus(arith1,arith2))
          loop(col+1)
        }
      }
    loop()
    res
  }

  // Overriding toString.
  override def toString() =
    this.foldLeft("")((string,c) =>
      string + "(" + c._1 + ", " + c._2.toString + ") "
    ) + " = " + b

  /** Returns the line in a LaTeX format in an equality representation.
   * The line is supposed corresponds to a tabular line.
   * @param cols the number of columns in the system.
   *             The line does not know it.
   * @return a latex tabular line corresponding to the line.
   */
  def toLatexEq(cols: Int): String = {
    val first = getFirstCoeff()._2
    // Directly starting at the first non zero.
    def loop(acc: Int = first,
             res: String = ""): String =
      if (acc <= cols) get(acc) match {
        case Some(coeff) =>
          loop(acc + 1,
               (if (res == "") "  " + (" & " * ((first - 1) * 2)) + "$"
                else res + " & $+$ & $") +
                     Mult(coeff,Ident("x_{" + acc + "}")).toLatex() + "$")
        case None =>
          loop(acc + 1, res + " & & ")
      }
      else res + " & $=$ & $" + b + "$\\\\[\\matandtabspace]"
    loop()
  }

  /** Returns the line in a LaTeX format in a matrix representation.
   * @param cols the number of columns in the system.
   *             The line does not know it.
   * @return a latex matrix line corresponding to the line.
   */
  def toLatexMatrix(cols: Int) = {
    // Directly starting at the first non zero.
    def loop(acc: Int = 1,
             res: String = ""): String =
      if (acc <= cols) get(acc) match {
        case Some(coeff) =>
          loop(acc + 1,
               (if (res == "") "  " + coeff.toLatex()
                else res + " & " + coeff.toLatex()))
        case None =>
          loop(acc + 1,
               (if (res == "") "  " + Zero.toLatex()
                else res + " & " + Zero.toLatex()))
      }
      else res + "\\\\[\\matandtabspace]"
    loop()
  }

  /** Returns b in the LaTeX format, surrounded by $-s. */
  def toLatexVector() = b.toLatex()
}

/** Class representing lines, i.e. the matrix itself.
 * Maps a row index to its line.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
class Lines extends OpenHashMap[Int,Line] {
  /** Returns a list of couple consisting of a line index
   * and the number of zeros at the beginning of this line.
   */
  def cardFirstZeros() =
    this.map(c => (c._1,c._2.cardFirstZeros())).toList

  /** Returns a representation of the lines as a list of String. */
  def toStringList() =
    this.map(c => c._2.toString).toList

  /** Returns a LaTeX representation of the lines as a tabular (equalities).
   * @param cols the number of columns in the system.
   *             The Lines does not know it.
   * @return a LaTeX tabular representing the lines.
   */
  def toLatexTab(cols: Int): List[String] = {
    object Sorter extends gausel.lib.InsertionSort[(Int,Line)] {
      def infEq1(l: (Int,Line), r: (Int,Line)) = l._1 <= r._1
      def infEq2(l: (Int,Line), r: (Int,Line)) = l._1 <= r._1
    }
    // The tabular has (cols * 2) - 1 + 2 colums:
    // - cols * 2 - 1 because each coefficient has a "+" right after it
    //   (cols * 2) except for the last one (- 1).
    // - + 2 because there is also the value to which the sum is equal to,
    //   preceeded by a "=", so two columns.
    // All the columns are centered.
    "\\begin{tabular}{" + (" c " * ((cols * 2) - 1 + 2)) + "}"::
    Sorter.sort1(toList).map({case (row,line) => line.toLatexEq(cols)}) ++
    ("\\end{tabular}"::Nil)
  }

  /** Returns a LaTeX representation of the lines as a matrix.
   * @param cols the number of columns in the system.
   *             The Lines does not know it.
   * @return the matrix, the variable vector and the "right side" vector
   */
  def toLatexMatrices(cols: Int) = {
    object Sorter extends gausel.lib.InsertionSort[(Int,Line)] {
      def infEq1(l: (Int,Line), r: (Int,Line)) = l._1 <= r._1
      def infEq2(l: (Int,Line), r: (Int,Line)) = l._1 <= r._1
    }
    val sortedLines = Sorter.sort1(toList)
    // The size needs not beeing specified on matrices.
    val matrix =
      "\\begin{pmatrix}"::
      sortedLines.map({case (row,line) => line.toLatexMatrix(cols)}) ++
      ("\\end{pmatrix}"::Nil)
    val variablesVector = 
      "\\begin{pmatrix}"::
      (for (i <- 1 to cols) yield "  x_" + i + "\\\\[\\matandtabspace]").toList ++
      ("\\end{pmatrix}"::Nil)
    val vector =
      "\\begin{pmatrix}"::
      sortedLines.map({case (row,line) => "  " + line.toLatexVector() + "\\\\[\\matandtabspace]"}) ++
      ("\\end{pmatrix}"::Nil)
    (matrix,variablesVector,vector)
  }
}

/** Class representing a linear system, presented in the traditional
 * A.x = b form where A is a matrix, b is a vector and A and b have
 * the same number of rows.
 * Note that x is not represented in the System. In fact its
 * coefficients correspond to the column indices in class Line and
 * are easily extracted afterwards, assuming no column permutation
 * takes place during the GE. It is the case here.
 * 
 * Contains many operations useful for Gaussian Elimination.
 * @param matrix the matrix (A) of the linear system
 * @param vector the vector (b) of the linear system
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
class System(matrix: List[List[Option[Arith]]],
             vector: List[Option[Arith]]) {

  /** Number of rows in the matrix. */
  val rows = matrix.length
  /** Number of columns in the matrix. */
  val cols = matrix.head.length

  /** Returns a copy of this system. */
  def copy() = new System(matrix,vector)

  /** Coefficients of the linear system. */
  private val coeffs = {
    // Result list to fill.
    val res = new Lines
    // Fills res.
    def loop(mat: List[List[Option[Arith]]] = matrix,
             vec: List[Option[Arith]] = vector,
             row: Int = 1): Unit = (mat,vec) match {
      case (h::t,hVec::tVec) => {
        // We have to build a new line.
        val map = new Line { val b = hVec match {
          case None => Zero
          case Some(arith) => arith
        } }
        // Fills map (the new line).
        def innerLoop(l: List[Option[Arith]] = h,
                      col: Int = 1): Unit = l match {
          case Some(arith)::t => {
            // Adding the new coefficient at its column index.
            map.update(col,arith)
            innerLoop(t,col+1)
          }
          case None::t => innerLoop(t,col+1)
          case Nil => ()
        }
        innerLoop()
        // Adding the new line it its row index.
        res.update(row,map)
        loop(t,tVec,row+1)
      }
      case (Nil,Nil) => ()
      case _ =>
        throw new Exception("Dimension mismatch; matrix has " + matrix.length +
                            " lines but vector has " + vector.length + ".")
    }
    // Filling res.
    loop()
    res
  }

  /** Checks if the system is triangular superior.
   * @return true iff the system is triangular
   */
  def isTriangular() = {
    def loop(acc: Int = 1): Boolean =
      if (acc >= rows) true
      else (coeffs get acc, coeffs get (acc+1)) match {
        case (Some(line),Some(linep1)) =>
          if (line.cardFirstZeros() < linep1.cardFirstZeros) loop(acc+1)
          else false
        case _ =>
          throw new Exception("Illegal row indices " + acc + " and " + (acc+1) + ", maximum is " + rows + ".")
      }
    loop()
  }

  /** Returns a list of couple consisting of a line index
   * and the number of zeros at the beginning of this line
   * for coeffs.
   */
  def cardFirstZeros() = coeffs.cardFirstZeros()

  /** Gets a line in coeffs.
   * @param row the desired line index
   * @return the corresponding line
   */
  def getLine(row: Int) =
    if (row > rows)
      throw new Exception("Illegal number of lines " + row + ", maximum is " + rows + ".")
    else coeffs get row match {
      case Some(line) => line
      case None => throw new Exception("Weird problem here.")
    }

  /** Gets a coefficient in the matrix.
   * @param row the row of the coefficient
   * @param col the column of the coefficient
   * @return the corresponding coefficient
   */
  def getCoef(row: Int, col: Int) = getLine(row) get col

  /** Reorganizes the lines with respect to the input list of integers.
   * @param newOrder the new order, the integers are the indices of the lines
   *        in the old order.
   */
  def reorganizeLines(newOrder: List[Int]) = {
    if (newOrder.length > rows)
      throw new Exception("Illegal number of lines " + newOrder.length + ", maximum is " + rows + ".")
    val oldCoeffs = coeffs.clone()
    def loop(l: List[Int] = newOrder,
             index: Int = 1): Unit = l match {
      case h::t => {
        coeffs.update(index,oldCoeffs.getOrElse(h,throw new Exception("Weird problem here.")))
        loop(t,index + 1)
      }
      case Nil => ()
    }
    loop()
  }

  /** Update a line in coeffs. */
  def updateLine(row: Int, newLine: Line) =
    if (row > rows)
      throw new Exception("Illegal number of lines " + row + ", maximum is " + rows + ".")
    else coeffs.update(row,newLine)

  /** Returns a representation of the linear system as a list of String. */
  def toStringList() =
    coeffs.toStringList()

  /** Returns a LaTeX representation of the system as a tabular (equalities).
   * @return a list of strings representing the lines of the LaTeX code
   *         corresponding to the system.
   */
  def toLatexTabular() = coeffs.toLatexTab(cols)

  /** Returns a LaTeX representation of the system as three matrices.
   * @return the matrix, the variable vector and the "right side" vector
   */
  def toLatexMatrices() = coeffs.toLatexMatrices(cols)

}
