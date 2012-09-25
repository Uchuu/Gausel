package gausel.ui

object Gausel extends App with gausel.lib.Verboser with gausel.lib.IOLib {

  // Verbose stuff.
  val name = "Gausel"
  val verbLevel = 1
  val color = cyan

  /** Verbose flag. */
  val verbFlag = "--verbose"
  /** Gausel legal flags. */
  val flags = verbFlag::Nil

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
    System.exit(0)
  }

  val inFull = cleanArgs(0)
  val (inPath,inFile,inNakedFile) = {
    val lastSlash = inFull.lastIndexOf('/')
    val file = inFull.drop(lastSlash+1)
    val lastDot = file.lastIndexOf('.')
    (inFull.take(lastSlash+1),
     file,
     if (lastDot == -1) file else file.take(file lastIndexOf '.'))
  }

  val outPath =
    if (cleanArgs.length > 1) cleanArgs(1)
    else inPath + inNakedFile + "gausel/"
  val outFile = inNakedFile + ".tex"

  verblnAlt("Input path: " + inPath)
  verblnAlt("Input file: " + inFile)
  verblnAlt("Output path: " + outPath)
  verblnAlt("Output file: " + outFile)
  verblnAlt("")

  val inLines = fileToList(inFull)

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

  verbln("Triangularizing the system:")
  solver.triangularize()
  solver.toStringList.foreach(l => verbln("  " + l))
  verbln(1)

  verbln("Extracting the result:")
  val output = solver.extractResult()
  output.solution.foreach(c => verbln("  " + c._1 + " => " + c._2))
  verbln(1)

  verbln("Almost done, generating the tex file in [" + outPath + outFile + "].")
  verbln("Creating the output folder.")
  mkdirs(outPath)
  val builder = new gausel.latex.LatexBuilder(title,output,rest)
  builder.writeToFile(outPath,outFile)
  verbln(1)

  verbln("Done, result file is in folder [" + outPath + outFile + "].")

  verbln(1)

  sys.exit(0)

}
