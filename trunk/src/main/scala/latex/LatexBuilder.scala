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

package gausel.latex

import  gausel.data.Output

/** Creates the LaTeX file synthesizing the resolution.
 * @param title The title of the LaTeX document.
 * @param output The output of the resolution.
 * @param text The content of the LaTeX document body.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
class LatexBuilder(val title: String,
                   val output: Output,
                   val text: List[String]) {

  def mkdirs(path: String) = (new java.io.File(path)).mkdirs

  val txt = text match {
    case Nil => DefaultText()
    case _ => text
  }

  //** Prefix of the skeleton of the LaTeX main file. *//
  val skelPrefix =
    "%=====================================================================%"::
    "%                  Setting the document class                         %"::
    "%=====================================================================%"::
    "\\documentclass[11pt,a4paper,oneside]{article}"::
    ""::
    "%=====================================================================%"::
    "%                        Packages import                              %"::
    "%=====================================================================%"::
    "\\usepackage{graphicx,amssymb,amstext,amsmath,amsthm,hyperref,indentfirst}"::
    "\\usepackage{array,amsfonts}"::
    "\\usepackage{xspace}"::
    "\\usepackage[utf8]{inputenc}"::
    "\\usepackage[T1]{fontenc}"::
    "\\usepackage{ulem}"::
    "\\usepackage{caption}"::
    ""::
    "%=====================================================================%"::
    "%                 Options to make stuff look pretty                   %"::
    "%=====================================================================%"::
    "\\hypersetup{"::
    "  pdfborder={0 0 0},"::
    "  colorlinks=true,"::
    "  linkcolor=blue,"::
    "  citecolor=blue,"::
    "  urlcolor=blue,"::
    "  pdftitle={"::
    ("    " + title)::
    "  }"::
    "}"::
    ""::
    Nil

  //** Suffix of the skeleton of the LaTeX main file. *//
  val skelSuffix =
    ""::
    "%==================================================================%"::
    "%                         Title and author(s)                      %"::
    "%==================================================================%"::
    "\\title{"::
    ("  " + title)::
    "}"::
    "\\author{"::
    "}"::
    ""::
    "%==================================================================%"::
    "%                    Let's start the document                      %"::
    "%==================================================================%"::
    "\\begin{document}"::
    "\\normalem"::
    ""::
    "%==================================================================%"::
    "%                              Title                               %"::
    "%==================================================================%"::
    "\\maketitle"::
    ""::
    "%==================================================================%"::
    "%                         Document body                            %"::
    "%==================================================================%"::
    ""::
    txt++
    (""::
    "%=================================================================%"::
    "%                      End of the Document                        %"::
    "%=================================================================%"::
    "\\end{document}"::
    Nil)

  def writeToFile(path: String,
                  file: String) = {
    mkdirs(path)
    val writer = new java.io.FileWriter(new java.io.File(if (path == "") file else path + "/" + file))
    skelPrefix.foreach(l => writer.write(l + "\n"))
    output.newCommands.foreach(l => writer.write(l + "\n"))
    skelSuffix.foreach(l => writer.write(l + "\n"))
    writer.close
  }

}

/** Default body of the generated LaTeX file.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object DefaultText {
  def apply() =
    ("\\section{Explanation}"::
     "The matrices, vectors, and systems are available to use anywhere in the tex file"::
     "using several commands. Sections~\\ref{original}~\\ref{triangle}"::
     "and~\\ref{solution} show how to use them in context."::
     "Please note that matrices and vectors have to be put in a math environment,"::
     "but tabulars do not.\\"::
     "Here are the commands defined:"::
     "\\begin{itemize}"::
     "\\item \\verb+\\originaltab+:\\\\ \\originaltab"::
     "\\item \\verb+\\originalmat+:\\\\ $\\originalmat$"::
     "\\item \\verb+\\originalvec+:\\\\ $\\originalvec$"::
     "\\item \\verb+\\triangletab+:\\\\ \\triangletab"::
     "\\item \\verb+\\trianglemat+:\\\\ $\\trianglemat$"::
     "\\item \\verb+\\trianglevec+:\\\\ $\\trianglevec$"::
     "\\item \\verb+\\variablevec+:\\\\ $\\variablevec$"::
     "\\item \\verb+\\solutiontab+:\\\\ \\solutiontab"::
     "\\item \\verb+\\solutionmat+:\\\\ $\\solutionmat$"::
     "\\end{itemize}"::
     "\\section{Original system}"::
     "\\label{original}"::
     "Assuming the unknown variable vector is represented as $\\variablevec$,"::
     "what we do is solve the following problem:"::
     "\\["::
     "\\originalmat"::
     " *"::
     "\\variablevec"::
     "="::
     "\\originalvec"::
     ". \\]"::
     "Equivalently, we can consider the following linear system:\\\\"::
     "\\originaltab"::
     ""::
     "\\section{Triangularized system}"::
     "\\label{triangle}"::
     "Now, using Gaussian Elimination we obtain the following matrix equation:"::
     "\\["::
     "\\trianglemat"::
     "*"::
     "\\variablevec"::
     "="::
     "\\trianglevec"::
     "\\]"::
     "Equivalently, the linear system:\\\\"::
     "\\triangletab"::
     ""::
     "\\section{Solution}"::
     "\\label{solution}"::
     "Now, the solution for the system described in Section~\\ref{original} and"::
     "triangularized in Section~\\ref{triangle} is:"::
     "\\[ \\solutionmat \\]"::
     "It can also be written as equalities:\\\\"::
     "\\solutiontab"::
     ""::
     "\\section{Conclusion}"::
     "I don't know who did this but it's pretty awesome."::
     "\\newline"::
     ""::
     "See ya."::
     Nil
   )
}
