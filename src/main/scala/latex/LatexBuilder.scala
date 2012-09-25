package gausel.latex

import  gausel.data.Output

class LatexBuilder(val title: String,
                   val output: Output,
                   val text: List[String]) extends gausel.lib.IOLib {

  val txt = text match {
    case Nil => DefaultText()
    case _ => text
  }

  //** Prefix of the skeleton of the LaTeX main file. *//
  val skelPrefix =
    "%=====================================================================%"::
    "%                        Making an report                             %"::
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
     "I don't know who did this but it's pretty fucking awesome."::
     "\\newline"::
     ""::
     "See ya."::
     Nil
   )
}
