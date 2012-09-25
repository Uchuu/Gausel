package gausel.test

import  gausel.data._
import  gausel.algo.Solver

object TestSolver extends App with gausel.lib.Verboser {

  // Verbose related stuff.
  val name = "TestSolver"
  val verbLevel =
    if (args.length > 0) try {
      args(0).toInt
    } catch {
      case e => 1
    } else 1
  val color = cyan
  val solverVerbLevel =
    if (args.length > 0) try {
      args(0).toInt
    } catch {
      case e => 0
    } else 0

  val path = "test"
  val file = "test.tex"

  // Creating a matrix as a List[List[Option[String]]].
  // Easily done by a parser reading a file.
  // Note that the matrix is filled, zeros are None-s but
  // they are present.
  val matrix1: List[List[Option[String]]] =
    (None        :: None        :: Some("a13") :: Nil) ::
    (Some("a21") :: Some("a22") :: None        :: Nil) ::
    (None        :: Some("a32") :: Some("a33") :: Nil) :: Nil
  val matrix2: List[List[Option[String]]] =
    (None        :: Some("a12") :: Some("a13") :: Nil) ::
    (Some("a21") :: Some("a22") :: None        :: Nil) ::
    (None        :: Some("a32") :: Some("a33") :: Nil) :: Nil
  // Creating the vector as a List[String].
  val vector : List[String] = "b1"::"b2"::"b3"::Nil
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
