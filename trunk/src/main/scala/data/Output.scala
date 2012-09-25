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

/** Data returned by the solver once it's done.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
class Output(val originalSystem: System,
             val triangularizedSystem: System,
             val solution: List[(Int,Arith)]) {

  /** Wraps the input command into a LaTeX new command.
   * @param label the label used to call the command
   * @param command the command
   * @param comment lines that will be commented and placed
   *                before the new command
   * @return a LaTeX new command
   */
  def newCommand(label: String,
                 command: List[String],
                 comment: String) =
    ("% " + comment)::
    ("\\newcommand\\" + label + "{")::
    command ++ ("}"::Nil)

  /** A LaTeX representation of the original system as a tabular. */
  val originalTab = originalSystem.toLatexTabular()

  /** A LaTeX representation of the original system as a tabular. */
  val originalMat = originalSystem.toLatexMatrices()

  /** A LaTeX representation of the triangularized system as a tabular. */
  val triangleTab = triangularizedSystem.toLatexTabular()

  /** A LaTeX representation of the triangularized system as a tabular. */
  val triangleMat = triangularizedSystem.toLatexMatrices()

  /** A LaTeX representation of the solution as a tabular. */
  val solutionTab =
    // Three columns:
    // - variable
    // - "="
    // - coeff
    // The first two ones are centered and the last one is left aligned.
    // /!\ The list is built in reverse order /!\
    ("\\end{tabular}"::
     solution.foldLeft[List[String]]("\\begin{tabular}{ c c l }"::Nil)((list,couple) =>
       "  $x_" + couple._1 + "$ & $=$ & $" + couple._2.toLatex() + "$\\\\[\\matandtabspace]"::list
     )
    ).reverse

  /** A LaTeX representation of the solution as a tabular. */
  val solutionMat =
    "\\begin{pmatrix}"::
    solution.map(c => "  " + c._2.toLatex() + "\\\\[\\matandtabspace]")++
    ("\\end{pmatrix}"::Nil)

  val newCommands =
    newCommand("matandtabspace",
               ".4em"::Nil,
               "Used to specify the space between the lines of the matrices and tabulars.")++
    newCommand("originaltab",originalTab,"Original system as a tabular.")++
    newCommand("originalmat",originalMat._1,"Original matrix.")++
    newCommand("originalvec",originalMat._3,"Original vector.")++
    newCommand("triangletab",triangleTab,"Triangularized system as a tabular.")++
    newCommand("trianglemat",triangleMat._1,"Triangularized matrix.")++
    newCommand("trianglevec",triangleMat._3,"Triangularized vector.")++
    newCommand("variablevec",originalMat._2,"Variable vector.")++
    newCommand("solutiontab",solutionTab,"Solution as a tabular.")++
    newCommand("solutionmat",solutionMat,"Solution as a vector.")

}
