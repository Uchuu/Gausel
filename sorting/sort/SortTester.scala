package sort

import scala.annotation.tailrec

/** Tests sorting algorithms. */
trait SortTester extends App with Verboser {

  type OptionDef = (String, String => Unit, String)

  sealed trait Result { val time: Long }
  case class Sorted(val time: Long) extends Result
  case class NotSorted(val time: Long) extends Result
  case class Timeout(val time: Long) extends Result
  object Overflow extends Result { val time: Long = -1 }

  def optionValue(s: String) = s.split("=")(1)

  object Options {
    private var _noReference = false
    def noReference_=(nu: Boolean) = _noReference = nu
    def noReference = _noReference
    private var _timeout = -1
    def timeout_=(to: Int) = _timeout = to
    def timeout = _timeout
    private var _max = 100
    def max_=(nuMax: Int) = _max = nuMax
    def max = _max
    // Gnuplot
    private var _gp: Option[String] = None
    def gp_=(nuGp: String) = _gp = Some(nuGp)
    def gp = _gp
    private var _keepGp = false
    def keepGp_=(nu: Boolean) = _keepGp = nu
    def keepGp = _keepGp
  }

  protected val name = Some("Sort Tester")
  protected val color = Cyan
  protected var verbLevel = 2
  def space(n: Int = 1) = if (verbLevel >= n) println
  def done(n: Int = 1) = verb("done.\n", n)
  def title(s: String, n: Int = 1) = verbln("|=======| " + s + " |=======|", n)

  val optionsDef: List[OptionDef] = {
    ("-h", {s: String => { printHelp() ; sys exit 0 }}, ": prints this message.") ::
    ("--help", {s: String => { printHelp() ; sys exit 0 }}, ": prints this message.") ::
    ("--noReference", { s: String => Options.noReference = true },
      ": prevents reference algorithms from running.") ::
    ("--to=", { s: String => Options.timeout = optionValue(s).toInt },
      "<int>: specify a timeout in milliseconds (default None).") ::
    ("--max=", { s: String => Options.max = optionValue(s).toInt },
      "<int>: maximum value of the random integers populating the lists (default 100).") ::
    ("--gp=", { s: String => Options.gp = optionValue(s) },
      "<path>: generates a gnuplot data file at the specified path.") ::
    ("--keepGp", { s: String => Options.gp = optionValue(s) },
      ": keeps the gnuplot script and data used to generate the graph.") ::
    Nil
  }

  def printHelp(s: String = "") = {
    space()
    if (s != "") verbln("\033[31;1m" + s + "\033[0m")
    space()
    verbln("Usage:")
    optionsDef foreach ({o: OptionDef => verbln("  " + o._1 + o._3)})
    space()
  }

  def error(s: String) = {
    printHelp(s) ; sys exit -1
  }

  @tailrec
  private def handleOptions(as: List[String]): Unit = as match {
    case opt :: t => {
      optionsDef find ({o: OptionDef => opt startsWith o._1}) match {
        case None => error("Unexpected option \"" + opt + "\".")
        case Some(option) => option._2(opt)
      }
      handleOptions(t)
    }
    case Nil => ()
  }

  val referenceAlgorithms = {
    uchuu.mergeSort ::
    uchuu.quickSort ::
    uchuu.insertionSort ::
    uchuu.bubbleSort ::
    Nil
  }
  val algorithms: List[Sort]
  val listSizes =
    10 :: 100 :: 1000 :: 10000 :: 100000 :: 1000000 :: Nil
  val maxInt = Options.max

  def run() = {
    handleOptions(args.toList)

    space() ; space()
    title("Starting.")

    space(2)
    verbln("Creating random lists.", 2)
    val toSort = listSizes map (size => {
      verbPrefix("> Of size ",2)
      if (verbLevel >= 2) printf("%15s", formatTime(size))
      verb(" ... ", 2)
      val result = Seq.fill(size)(scala.util.Random.nextInt(maxInt)).toList
      done()
      result
    })
    verbln("Done.")

    space()
    title("Testing.")
    space()
    toSort foreach (list => test(list))
    verbln("Done.")
    Options.gp match {
      case None => ()
      case Some(path) => {
        verbln("")
        BenchResult.toGnuplot(path)
      }
    }
    verbln("Exiting.")
    space()
  }

  def test(list: List[Element]) = {
    verbln("Testing with a list of size \033[1m" + formatTime(list.size) + "\033[0m.")
    if (!Options.noReference) referenceAlgorithms foreach (algo => oneBench(list,algo,Blue))
    algorithms foreach (algo => oneBench(list,algo,Magenta))
  }

  def oneBench(list: List[Element], algo: Sort, color: Color) = {
    val result: Result = try {
      val startTime = System.currentTimeMillis
      val resultList = algo(list,Options.timeout)
      val time = System.currentTimeMillis - startTime
      if (isSorted(resultList,list)) Sorted(time) else NotSorted(time)
    } catch {
      case TimeoutException(to) => Timeout(to)
      case e: StackOverflowError => Overflow
    }
    BenchResult.log(list.size,algo,result)
    printFormat(algo.name, algo.author, result, color)
  }

  def formatTime(time: Long) = {
    val string = time.toString
    val firstChars = string.size % 3
    @tailrec
    def loop(s: String, prefix: String): String = s.size match {
      case n if (n > 0) && (prefix != "") => loop(s drop 3, prefix + "," + (s take 3))
      case n if n > 0 => loop(s drop 3, (s take 3))
      case 0 => prefix
      case _ => throw new Exception("Unexpected string length " + s + ".")
    }
    if (string.size == 3) string
    else loop(string drop firstChars, string take firstChars)
  }

  def printFormat(
    name: String, author: String, result: Result, authorColor: Color
  ) = {
    val (sortedString,sortedColor) = result match {
      case Timeout(_) => ("timeout", Red.toString)
      case Sorted(_) => ("sorted",Cyan.toString)
      case NotSorted(_) => ("not sorted",Red.toString)
      case Overflow => ("overflow",Red.toString)
    }
    val timeString = formatTime(result.time)
    verbPrefix("")
    printf(
      "\033[1m%30s\033[0m by \033[%s;1m%-10s\033[0m | \033[%s;1m%10s\033[0m | %20s ms\n",
      name, authorColor, author, sortedColor, sortedString, timeString
    )
  }

  object BenchResult {
    import scala.collection.mutable.{HashMap,Map}
    private val map = new HashMap[Int,Map[Sort,Result]]
    def log(size: Int, algo: Sort, result: Result) = map get size match {
      case Some(m) => map.update(size, m.updated(algo,result))
      case None => map.update(size, (new HashMap[Sort,Result]).updated(algo,result))
    }
    def get(size: Int): Map[Sort,Result] = (map get size).get
    def get(algo: Sort, map: Map[Sort,Result]) = (map get algo).get

    def toGnuplot(path: String) = {
      import java.io.{FileWriter,BufferedWriter}

      def printData(dataPath: String) = {
        val bw = new BufferedWriter(new FileWriter(dataPath))
        def write(s: String) = bw write s
        def writeln(s: String) = { write(s) ; write("\n") }
        def flush = bw.flush
        def close = bw.close

        def printKeys = {
          write("## size ")
          (referenceAlgorithms ++ algorithms) foreach (algo => {
            write("\"") ; write(algo.name) ; write(" by ") ; write(algo.author) ; write("\" ")
          })
          writeln("")
          flush
        }

        def printForSize(size: Int) = {
          val thisSizeMap = get(size)
          // Printing the list size in the first column.
          write(formatTime(size.toLong)) ; write(" ")
          // Printing each algorithms results (in order).
          (referenceAlgorithms ++ algorithms) foreach (algo => get(algo,thisSizeMap) match {
            case NotSorted(_) => write("nan ")
            case Overflow => write("nan ")
            case res => { write(res.time.toString) ; write(" ") }
          })
          writeln("")
          flush
        }
        printKeys
        listSizes foreach (size => printForSize(size))
        close
      }

      def printScript(scriptPath: String, dataPath: String) = {
        val bw = new BufferedWriter(new FileWriter(scriptPath))
        def write(s: String) = bw write s
        def writeln(s: String) = { write(s) ; write("\n") }
        def flush = bw.flush
        def close = bw.close

        // Writing script config.
        write("""#!/usr/bin/gnuplot
reset
set terminal pngcairo size 1000,600
set output """")
        write(path) ; write(".png")
        write(""""

set size ratio 1
set key inside right top vertical Right noreverse enhanced autotitles columnhead nobox
set style data linespoints

set datafile missing "?"

set xlabel "size of the list"
set xtics rotate by -45
set ylabel "time in ms """)
        Options.timeout match {
          case n if n <= 0 => write("(no timeout)\"\n")
          case n => { write("(timeout ") ; write(formatTime(n)) ; write(" ms)\"\n") }
        }
        write("""
set title "Comparisons of sorting algorithms."

set key outside right top Left
set grid

set xtics nomirror
""")

        @tailrec
        def loop(
          algos: List[Sort] = referenceAlgorithms ++ algorithms, column: Int = 2, prefix: String = "  \"\" u "
        ): Unit = algos match {
          case algo :: t => {
            if (column > 2) write(", \\")
            writeln("") ; write(prefix) ; write(column.toString) ; write(":xticlabels(1) w lp lw 2 title \"")
            write(algo.name) ; write(" by ") ; write(algo.author) ; write("\"")
            loop(t,column+1)
          }
          case Nil => { writeln("") ; writeln("#") }
        }

        loop(prefix = "plot \"" + dataPath + "\" u ")
        flush
        close
      }

      import scala.sys.process._

      verbPrefix("Generating gnuplot data file [" + path + "]... ")
      printData(path)
      done()

      verbPrefix("Generating gnuplot script [" + path + ".gp]... ")
      printScript(path + ".gp", path)
      done()

      val logger = ProcessLogger((o: String) => (), (e: String) => ())

      verbPrefix("Making gnuplot script executable... ")
      val chmod = "chmod u+x " + path + ".gp"
      chmod ! logger
      done()
      verbln("Command used: [" + chmod + "].")

      verbPrefix("Generating graph as a png file [" + path + ".png]... ")
      val gnuplot = "./" + path + ".gp"
      gnuplot ! logger
      done()
      verbln("Command used: [" + gnuplot + "].")

      if (!Options.keepGp) {
        verbPrefix("Deleting [" + path + "] and [" + path + ".gp] used to create the graph... ")
        val rm = "rm " + path + " " + path + ".gp"
        rm ! logger
        done()
      }

      verbln("")
    }
  }
}
