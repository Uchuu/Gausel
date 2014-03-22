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

import  gausel.data._
import  gausel.algo.Solver

/** Launches a simple resolution.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object TestSolver extends App with gausel.lib.Verb {

  // Verbose related stuff.
  val name = "TestSolver"
  val verbLevel =
    if (args.length > 0) try {
      args(0).toInt
    } catch {
      case e: Throwable => 1
    } else 1
  val color = Colors.cyan
  val solverVerbLevel =
    if (args.length > 0) try {
      args(0).toInt
    } catch {
      case e: Throwable => 0
    } else 0

  val path = "test"
  val file = "test.tex"

  // Creating a matrix as a List[List[Option[String]]].
  // Easily done by a parser reading a file.
  // Note that the matrix is filled, zeros are None-s but
  // they are present.
  val (a12,a13,a21,a22,a32,a33) =
    (Ident("a12"), Ident("a13"), Ident("a21"), Ident("a22"), Ident("a32"), Ident("a33"))
  val (b1,b2,b3) = (Ident("b1"), Ident("b2"), Ident("b3"))
  val matrix1: List[List[Option[Arith]]] =
    (None      :: None      :: Some(a13) :: Nil) ::
    (Some(a21) :: Some(a22) :: None      :: Nil) ::
    (None      :: Some(a32) :: Some(a33) :: Nil) :: Nil
  val matrix2: List[List[Option[Arith]]] =
    (None      :: Some(a12) :: Some(a13) :: Nil) ::
    (Some(a21) :: Some(a22) :: None      :: Nil) ::
    (None      :: Some(a32) :: Some(a33) :: Nil) :: Nil
  // Creating the vector as a List[String].
  val vector : List[Option[Arith]] = Some(b1) :: Some(b2) :: Some(b3) :: Nil
  // Creating the actual system class.
  val system = new System(matrix2,vector)
  val systemForSolver = new System(matrix2,vector)
  val solver = new Solver(systemForSolver,solverVerbLevel)

  /** Prints the system with each line prefixed by two spaces. */
  def verbSystem() = {
    verbln("System:")
    // verbln("  " + system.toStringList)
    verbList(system.toStringList.map("  " +_),1)
    verbln(1)
  }

  /** Prints the solver with each line prefixed by two spaces. */
  def verbSolver() = {
    verbln("Solver state:")
    verbList(solver.toStringList.map("  " +_),1)
    verbln(1)
  }

  verbln(1)
  verbSystem()

  verbSolver()

  verbln("Now triangularizing the system.")
  verbln(1)
  solver.triangularize()
  verbSolver()

  verbln("Alright, now extracting the result.")
  verbln(1)
  val output = solver.extractResult()
  verbln("Done:")
  output.solution.foreach(c => verbln("  " + c._1 + " => " + c._2))
  verbln(1)

  import gausel.latex._

  verbln("Good, writing the LaTeX file in [" + path + "/" + file + "].")
  val builder = new LatexBuilder("One two one two it's just a test.",
                                 output,
                                 DefaultText())
  builder.writeToFile(path,file)

  verbln("Meow.")
  verbln("See ya.")
  verbln(1)

}
