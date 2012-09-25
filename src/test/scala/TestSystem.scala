package gausel.test

import  gausel.data._

object TestSystem extends App with gausel.lib.Verboser {

  // Verbose related stuff.
  val name = "TestSystem"
  val verbLevel = 1
  val color = cyan

  // Creating a matrix as a List[List[Option[String]]].
  // Easily done by a parser reading a file.
  // Note that the matrix is filled, zeros are None-s but
  // they are present.
  val (a13,a21,a22,a32,a33) =
    (Ident("a13"),Ident("a21"),Ident("a22"),Ident("a32"),Ident("a33"))
  val (b1,b2,b3) =
    (Ident("b1"),Ident("b2"),Ident("b3"))
  val matrix: List[List[Option[String]]] =
    (None      :: None      :: Some(a13) :: Nil) ::
    (Some(a21) :: Some(a22) :: None      :: Nil) ::
    (None      :: Some(a32) :: Some(a33) :: Nil) :: Nil
  // Creating the vector as a List[String].
  val vector : List[String] = b1::b2::b3::Nil
  // Creating the actual system class.
  val system = new System(matrix,vector)

  /** Prints the system with each line prefixed by two spaces. */
  def verbSystem() = {
    verbln("System:")
    // verbln("  " + system.toStringList)
    verbList(system.toStringList.map("  " +_),1)
    verbln(1)
  }

  verbln(1)
  verbSystem()
  verbln("Testing cardFirstZeros.")
  verbln("  " + system.cardFirstZeros())
  verbln(1)

  val newOrder = 2::3::1::Nil
  verbln("Testing line reorganization: " + newOrder + ".")
  system reorganizeLines newOrder
  verbSystem()

  verbln("Retesting cardFirstZeros.")
  verbln("  " + system.cardFirstZeros())
  verbln(1)

  val (line1,line2) = (system.getLine(1),system.getLine(2))
  val factor = Ident("[factor]")
  verbln("Testing line multiplication:")
  verbln("          " + line1)
  verbln("  times   " + factor)
  val factorLine1 = line1 mult factor
  verbln("  result: " + factorLine1)
  verbln(1)

  verbln("Testing line substraction:")
  verbln("          " + line2)
  verbln("  minus   " + factorLine1)
  val subLine = line2 minus factorLine1
  verbln("  result: " + subLine)
  verbln(1)

  verbln("Now updating line 2 of the system with the result.")
  system.updateLine(2,subLine)
  verbSystem

  verbln("That was pretty nice, I'm done.")
  verbln("See ya.")
  verbln(1)

}
