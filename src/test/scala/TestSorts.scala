package gausel.test

object TestSorts extends App with gausel.lib.Verboser with gausel.lib.InsertionSort[Int] {

  // Verbose related stuff.
  val name = "TestSorts"
  val verbLevel =
    if (args.length > 0) try {
      args(0).toInt
    } catch {
      case e => 1
    } else 1
  val color = cyan

  // Checks if a list of integers is sorted.
  def isSorted(l: List[Int]): Boolean = l match {
    case h1::h2::t if h1 <= h2 => isSorted(h2::t)
    case h1::h2::t => {
      verbln("False: " + h1 + " > " + h2 + ".")
      false
    }
    case h::Nil => true
    case Nil => true
  }

  // Random stuff.
  val randomer = new scala.util.Random()

  // Comparison relations.
  def infEq1(l: Int, r: Int) = l <= r
  def infEq2(l: Int, r: Int) = l <= r

  // Beginning testing.
  verbln(1)
  verbln("List to sort:")
  val toSort = (for (i <- 1 to 20) yield randomer.nextInt(800)).toList
  verbln("  " + toSort)
  verbln("Result:")
  val result = sort1(toSort)
  verbln("  " + result)
  verbln(1)

  val bigToSort = (for (i <- 1 to 1000) yield randomer.nextInt(800)).toList
  verbln("Trying on a big random list (length: " + bigToSort.length + ").")
  val bigStartTime = System.currentTimeMillis()
  val bigResult = sort1(bigToSort)
  val bigEndTime = System.currentTimeMillis()
  verbln("Done in  " + (bigEndTime - bigStartTime) + "ms.")
  if (bigResult.length == bigToSort.length) {
    verbln("Same number of elements.")
    if (isSorted(bigResult))
      verbln("Result list is sorted.")
    else
      verbln("Result list is not sorted.")
  } else
    verbln("Both lists do not have the same number of elements (" +
           bigToSort.length + "/" + bigResult.length + ").")
  verbln("  " + bigResult,2)
  verbln(1)

  val biggerToSort = (for (i <- 1 to 10000) yield randomer.nextInt(1000)).toList
  verbln("Trying on a bigger random list (length: " + biggerToSort.length + ").")
  val biggerStartTime = System.currentTimeMillis()
  val biggerResult = sort1(biggerToSort)
  val biggerEndTime = System.currentTimeMillis()
  verbln("Done in  " + (biggerEndTime - biggerStartTime) + "ms.")
  if (biggerResult.length == biggerToSort.length) {
    verbln("Same number of elements.")
    if (isSorted(biggerResult))
      verbln("Result list is sorted.")
    else
      verbln("Result list is not sorted.")
  } else
    verbln("Both lists do not have the same number of elements (" +
           biggerToSort.length + "/" + biggerResult.length + ").")
  verbln("  " + biggerResult,2)
  verbln(1)

  verbln("That's all.")
  verbln("See ya.")
  verbln(1)

}
