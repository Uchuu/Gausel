package gausel.test

object TestParser extends App with gausel.lib.Verboser {

  // Verbose related stuff.
  val name = "TestParser"
  val verbLevel = 1
  val color = cyan

  val toParse =
    "title {"::
    "  One two one two it's just a test."::
    "}"::
    ""::
    "matrix {"::
    "  0 a12 a13"::
    "  a21 a22 0"::
    "  0 a32 a33"::
    "}"::
    ""::
    "vector {"::
    "  b1"::
    "  b2"::
    "  b3"::
    "}"::
    ""::
    "Bla bla bla"::
    "Toto tata titi tutu"::
    "sntahoestnud;ad,.tnd"::
    ""::
    ";s,.ntphaoearicd"::
    Nil

  verbln(1)
  verbln("Parsing the following lines:")
  toParse.foreach(l => verbln("  " + l))
  val (title,matrix,vector,rest) =
    gausel.parser.SystemParser(toParse)
  verbln(1)
  verbln("Result:")
  verbln("  title:")
  verbln("    " + title)
  verbln("  matrix:")
  matrix.foreach(l => verbln("    " + l))
  verbln("  vector:")
  vector.foreach(l => verbln("    " + l))
  verbln("  rest:")
  rest.foreach(l => verbln("    " + l))
  verbln(1)

  verbln("That's all.")
  verbln("See ya.")

}
