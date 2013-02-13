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

package gausel.ui

/** Gausel entry point.
 * Handles the user input and launches the resolution.
 * 
 * @author dameNingen <dame.ningen@mail.com>
 * @version $Revision$
 * $Id$
 */
object Gausel extends App with gausel.lib.Verb {

  /** Verbose flag. */
  val verbFlag = "--verbose"
  /** Gausel legal flags. */
  val flags = verbFlag::Nil

  // Verbose stuff.
  val name = "Gausel"
  val verbLevel = if (args contains verbFlag) 1 else 0

  /** Contains the usage instructions. */
  val help =
    ("Usage: gausel <file> [output]:"::
     " - <file>: the path to the file to process (mandatory),"::
     " - [output]: a path to a directory where the output will be stored"::
     "   (optional). If blank, files will be put in a directory named gausel"::
     "   in the same directory as the input file."::
     Nil)

  /** Prints the usage instructions. */
  def printHelp() = {
    verbln(1)
    help.foreach(l => verbln(l))
    verbln(1)
  }

  verbln(1)

  /** Indicates if the run is verbose or not. */
  val verbose = args.contains(verbFlag)
  if (args.contains("-help") || args.contains("--help") || args.contains("-h") || args.contains("--h")) {
    printHelp()
    sys.exit(0)
  }

  def verblnAlt(s: => String) = if (verbose) verbln(s)

  /** The list of arguments without the flags. */
  val cleanArgs = args.filterNot(s => s.head == '-')

  if (cleanArgs.length < 1) {
    verbln("Error: no parameter detected.")
    printHelp()
    sys exit 0
  }

  val inFull = cleanArgs(0) map (
    char => if (char == '\\') '/' else char
  )
  val (inPath,inFile,inNakedFile) = {
    val lastSlash = inFull.lastIndexOf('/')
    val file = inFull.drop(lastSlash+1)
    val lastDot = file.lastIndexOf('.')
    (inFull.take(lastSlash+1),
     file,
     if (lastDot == -1) file else file.take(file lastIndexOf '.'))
  }

  val outPath =
    if (cleanArgs.length > 1) {
      val temp = cleanArgs(1)
      if (temp endsWith "/") temp
      else temp + "/"
    } else inPath + "gausel/"
  val outFile = inNakedFile + ".tex"

  verblnAlt("Input path: " + inPath)
  verblnAlt("Input file: " + inFile)
  verblnAlt("Output path: " + outPath)
  verblnAlt("Output file: " + outFile)
  verblnAlt("")

  val inLines = {
    val reader = new java.io.BufferedReader(new java.io.FileReader(inFull))
    def loop(res: List[String] = Nil): List[String] = {
      val line = reader.readLine()
      if (line == null) res.reverse
      else loop(line::res)
    }
    loop()
  }

  verblnAlt("Parsing the input file:")
  val (title,matrix,vector,rest) = gausel.parser.SystemParser(inLines)
  verblnAlt("  title:")
  verblnAlt("    " + title)
  verblnAlt("  matrix:")
  matrix.foreach(l => verblnAlt("    " + l))
  verblnAlt("  vector:")
  vector.foreach(l => verblnAlt("    " + l))
  verblnAlt("  rest:")
  rest.foreach(l => verblnAlt("    " + l))
  verblnAlt("")

  verbln("Creating the solver. System:")
  val solver = new gausel.algo.Solver(new gausel.data.System(matrix,vector))
  solver.toStringList.foreach(l => verbln("  " + l))
  verbln(1)

  val startTime = System.currentTimeMillis

  verbln("Triangularizing the system:")
  solver.triangularize()
  solver.toStringList.foreach(l => verbln("  " + l))
  verbln(1)

  verbln("Extracting the result:")
  val output = solver.extractResult()
  output.solution.foreach(c => verbln("  " + c._1 + " => " + c._2))
  verbln(1)

  val time = (System.currentTimeMillis - startTime) / 1000

  verbln("Almost done, generating the tex file in [" + outPath + outFile + "].")
  // verbln("Creating the output folder.")
  // mkdirs(outPath)
  val builder = new gausel.latex.LatexBuilder(title,output,rest)
  builder.writeToFile(outPath,outFile)

  verbln(0)
  verbln(0)
  verbln("Done, resolution took " + (time.toString) + " seconds.",0)
  verbln("Tex file generated at [" + outPath + outFile + "].",0)
  verbln("To generate the pdf file, run",0)
  verbln("  \033[31;1mpdflatex " + outPath + outFile + "\033[0m",0)
  verbln(0)
  verbln(0)

  sys exit 0

}
