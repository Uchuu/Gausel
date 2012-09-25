package gausel.algo

import  scala.collection.mutable.OpenHashMap

import  gausel.data._

class Solver(val originalSystem: System,
             val verbLevel: Int = 0) extends gausel.lib.InsertionSort[(Int,Int)] with gausel.lib.Verboser {

  /** The system we will be working on.
   * The original one is kept for bookkeeping.
   */
  val system = originalSystem.copy()

  // Verbose related stuff.
  val name = "Solver"
  val color = blue

  def infEq1(l: (Int,Int), r: (Int,Int)) = l._1 <= r._1
  def infEq2(l: (Int,Int), r: (Int,Int)) = l._2 <= r._2

  /** Makes the system triangular superior. */
  def triangularize(): Unit = {
    verbln("Beginning triangularization.")
    verbln("Sorting the system.")
    sortSystem()
    verbSolver(2)
    if (!system.isTriangular()) {
      verbln("System is not triangular.")
      val cardFirstZeros = system.cardFirstZeros()
      // Finds the first two lines with the same number of first zeros in cardFirstZeros.
      // Returns both lines row index and the index of the first non zero.
      def loop(zeros: List[(Int,Int)] = sort1(cardFirstZeros)): (Int,Int,Int) = zeros match {
        case h1::h2::t =>
          if (h1._2 == h2._2) (h1._1,h2._1,h1._2+1)
          else loop(h2::t)
        case _ => throw new Exception("Could not find two consecutive lines with the same number of first zeros.")
      }
      val (i1,i2,col) = loop()
      verbln("Pivoting on lines " + i1 + " and " + i2 + ": first non zero: " + col + ".")
      // Getting the lines.
      val (line1,line2) = (system.getLine(i1),system.getLine(i2))
        // Building the factor.
        val factor = Div(line2.getOrElse(col,throw new Exception("Weird.")),
                         line1.getOrElse(col,throw new Exception("Weird.")))
      verbln("Replacing line " + i2 + " by (line" + i2 + " - " +
             factor + " * line" + i1 + ".",2)
      // Creating the new line.
      val newLine2 = line2 minus (line1 mult factor)
      verbln("Temporary line" + i2 + ", system does not know the first non zero is " +
             "actually a zero:",2)
      verbln("  " + newLine2 + ".",2)
      // We have to tell it that coefficient at 'col' is zero.
      newLine2 removeCoeff col
      system.updateLine(i2,newLine2)
      verbSolver(2)
      verbln("Done, iterating.")
      verbln(1)
      triangularize()
    } else {
      verbln("System is triangular.")
      verbSolver(2)
      verbln(1)
    }
  }

  /** Extracts the values of the unknown variables of the system.
   * Alright so this is the tricky part.
   * The first non zero coefficient of the line is normally what we want the
   * value of. However, it can be the case  that we don't know the values of
   * the unknown variables appearing in this line. For example, for a system
   * such as
   * a.x1 + b.x2 + c.x3 = b1
   *        e.x2 + f.x3 = b2
   * then we have to return a constraint, e.x2 + f.x3 = b2 here.
   * This corresponds to uninversible systems, and is not implemented yet.
   * @return an Output containing all the information about the resolution
   */
  def extractResult() = {
    // Iterates on the lines starting from the last one.
    def loop(acc: Int = system.rows,
             res: OpenHashMap[Int,Arith] = new OpenHashMap): OpenHashMap[Int,Arith] =
      if (acc < 1) {
        verbln("Done.",2)
        verbln(2)
        res
      }
      else {
        verbln("Handling line " + acc + ".",2)
        val line = system.getLine(acc)
        // This is the index of the variable we will extract.
        val firstIndex = line.getFirstCoeff()._2
        if (line.forall({
          case (index,coeff) => index == firstIndex || res.isDefinedAt(index)
        })) {
          val value = extractValue(line,res)
          res.update(firstIndex,value)
          verbln(2)
          loop(acc-1,res)
        } else
          throw new UnimplementedException("uninversible system.")
      }

    // Extracts the value of the first non zero of a map.
    def extractValue(line: Line, map: OpenHashMap[Int,Arith]): Arith = {
      val (coeff,index) = line.getFirstCoeff()
      verbln("Extracting value for unknown variable " + index + ".",2)
      val otherCoeffs = line.foldLeft[List[Arith]](Nil)((list,couple) => {
        // If the variable is the one we are extracting, we skip it.
        if (couple._1 == index) list
        else
          Mult(couple._2,
               map.getOrElse(couple._1,
                             throw new Exception("Don't know the value of " +
                                                 "variable " + couple._1 + ".")))::list
      })
      val value = otherCoeffs match {
        case Nil => Div(line.b,coeff)
        case _ => Div(otherCoeffs.foldLeft(line.b)((left,right) => Minus(left,right)),coeff)
          // Div(Minus1N(line.b,otherCoeffs),coeff)
      }
      verbln("Value for unknown variable " + index + ":",3)
      verbln("  " + value + ".",3)
      value
    }

    verbln("Extracting the result.")
    object Sorter extends gausel.lib.InsertionSort[(Int,Arith)] {
      def infEq1(l: (Int,Arith), r: (Int,Arith)) = l._1 <= r._1
      def infEq2(l: (Int,Arith), r: (Int,Arith)) = l._1 <= r._1
    }
    new Output(originalSystem,system,Sorter.sort1(loop().toList))
  }

  /** Sorts the system so that the lines are by increasing number of first zeros. */
  def sortSystem() = {
    val cardFirstZeros = system.cardFirstZeros()
    // Sorting the (line,firstZeros) based on firstZeros,
    // keeping only the line numbers.
    // See src/test/scala/TestSystem.scala.
    val newOrder = sort2(cardFirstZeros).map(_._1)
    system reorganizeLines newOrder
  }

  /** Prints the solver with each line prefixed by two spaces. */
  def verbSolver(v: Int = 1) = {
    verbln("State:",v)
    verbList(toStringList.map("  " +_),v)
  }

  def toStringList() = system.toStringList()

}

case class UnimplementedException(s: String) extends Exception("Uninplemented: " + s)
