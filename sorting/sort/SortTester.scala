package sort

import scala.annotation.tailrec

/** Tests sorting algorithms. */
trait SortTester extends App with Verboser {

  def optionValue(s: String) = s.split("=")(1)

  protected val name = Some("Sort Tester")
  protected val color = Cyan
  protected var verbLevel = 2
  def space(n: Int = 1) = verbln("",n)
  def title(s: String, n: Int = 1) = verbln("|=======| " + s + " |=======|", n)

  val referenceAlgorithms =
    uchuu.quickSort :: Nil // uchuu.insertionSort :: Nil
  val algorithms: List[Sort]
  val listSizes =
    10 :: 100 :: 1000 :: 10000 :: 100000 :: 1000000 :: 10000000 :: Nil
  val maxInt = 100

  def run() = {
    space() ; space()
    title("Starting.")

    space(2)
    verbln("Creating random lists.", 2)
    val toSort = listSizes map (size => {
      verbPrefix("> Of size " + size + " ... ", 2)
      val result = Seq.fill(size)(scala.util.Random.nextInt(maxInt)).toList
      verb("done.\n", 2)
      result
    })
    verbln("Done.")

    space()
    title("Testing.")
    space()
    toSort foreach (list => test(list))
    verbln("Done.")
    verbln("Exiting.")
    space()
  }

  def test(list: List[Element]) = {
    verbln("Testing with a list of size \033[1m" + list.size + "\033[0m.")
    referenceAlgorithms foreach (algo => oneBench(list,algo,Blue))
    algorithms foreach (algo => oneBench(list,algo,Magenta))
  }

  def oneBench(list: List[Element], algo: Sort, color: Color) = {
    val startTime = System.currentTimeMillis
    val result = algo(list)
    val time = System.currentTimeMillis - startTime
    val sorted = isSorted(result)
    printFormat(
      algo.name, algo.author, sorted, time, color
    )
  }

  def formatTime(time: Long) = {
    val string = time.toString
    val firstChars = string.size % 3
    @tailrec
    def loop(s: String, prefix: String): String = s.size match {
      case n if n > 0 => loop(s drop 3, prefix + "," + (s take 3))
      case 0 => prefix
      case _ => throw new Exception("Unexpected string length " + s + ".")
    }
    if (string.size == 3) string
    else loop(string drop firstChars, string take firstChars)
  }

  def printFormat(
    name: String, author: String, sorted: Boolean, time: Long, authorColor: Color
  ) = {
    val (sortedString,sortedColor) =
      if (sorted) ("sorted",Cyan.toString) else ("not sorted",Red.toString)
    val timeString = formatTime(time)
    verbPrefix("")
    printf(
      "%30s by \033[%s;1m%-10s\033[0m | \033[%s;1m%10s\033[0m | %20s ms\n",
      name, authorColor, author, sortedColor, sortedString, timeString
    )
  }
}
