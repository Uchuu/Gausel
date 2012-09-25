package gausel.lib

trait Header {
  protected val headerBound = "|"
  protected val headerFill = "="
  protected val headerSep = " "
  protected val headerLength = 100
  /** Creates a nice looking header of 80 characters.
   * Can be tuned by overridding the '''header''' prefixed values.
   * @param s the <code>String</code> around which the header will be created.
   * @return the header.
   */
  def header(s: String) = {
    if (s.length < (headerLength - (4 * headerBound.length) - (2 * headerSep.length))) {
      val fill = (headerLength - (4 * headerBound.length) - s.length - (2 * headerSep.length)) / 2
      headerBound + (headerFill * fill) + headerBound + headerSep + s + headerSep + headerBound + (headerFill * fill) + headerBound
    } else s
  }
}

trait Colors {
  /** Returns a random color integer used for the color of the name when printing it.
   * The color cannot be black (30) nor white (37).
   * @return a random integer between 31 and 36.
   */
  protected def getColor() =
    31 + (new scala.util.Random()).nextInt(6)

  protected val red = 31
  protected val green = 32
  protected val yellow = 33
  protected val blue = 34
  protected val magenta = 35
  protected val cyan = 36
}

/** Standard stuff trait for the verbose methods.
 * Three fields are needed:
 *  - verbLevel the verbose level used at runtime to know what to print,
 *  - name will be displayed as a <code>"[" + name + "]"</code> prefix before the actual <code>String</code>-s to print,
 *  - color the color used when printing <code>name</code>.
 *    Call getColor for a random color (will not be white nor black).
 * Everything in here is <code>protected</code> except for <code>name</code>.
 *
 * @author Adrien Champion
 */
trait Verboser extends Colors with Header {

  /** The verbose level used at runtime to know what to print. */
  protected val verbLevel: Int

  /** The name of the instance/object. */
  val name: String

  /** The color used for displaying the name. */
  protected val color: Int

  /** Prefix used for verbose.
   * @return the prefix with respect to <code>name</code> and <code>color</code>.
   */
  protected def prefix() =
    "[\033[1;" + color + "m" + name + "\033[0;00m] "

  /** Prints a <code>String</code> iff <code>verbLevel</code> is greater than or equal to the <code>verbose</code> parameter.
   * Does not print the prefix.
   * @param s the string to print (lazy).
   * @param verbose the verbose level above which <code>s</code> will be printed, default <code>1</code>.
   *   The <code>String</code> will always be printed if set to <code>-1</code>.
   */
  protected def verb(s: => String, verbose: Int = 1) =
    if (verbLevel >= verbose || verbose == -1) print(s)

  /** Prints a <code>String</code> preceded by the prefix given by the <code>prefix</code> function followed
   * by a newline iff <code>verbLevel</code> is greater than or equal to the <code>verbose</code> parameter.
   * @param s the string to print (lazy).
   * @param verbose the verbose level above which <code>s</code> will be printed, default <code>1</code>.
   *   The <code>String</code> will always be printed if set to <code>-1</code>.
   */
  protected def verbln(s: => String, verbose: Int = 1) =
    verb(prefix + s + "\n", verbose)

  /** Prints a newline iff <code>verbLevel</code> is greater than or equal to the <code>verbose</code> parameter.
   * Does not print the prefix.
   * @param verbose the verbose level above which the new line will be printed.
   *   The new line will always be printed if set to <code>-1</code>.
   */
  protected def verbln(verbose: Int) =
    verb("\n",verbose)

  protected def verbList(ls: => List[String], verbose: Int) =
    if (verbLevel >= verbose || verbose == -1)
      for (s <- ls) println(prefix + s)

}

/** Object containing IO related functions.
 * @author Adrien Champion
 */
trait IOLib {
  import java.io._

  /** Deletes a directory of a file.
   * @param path the path to the directory or the file to delete.
   */
  def delete(path: String) = {
    def loop(path: String,
             filesDirs: List[(String,String)],
             delSuccess: Boolean = true,
             stack: Boolean => Boolean = b => b): Boolean = filesDirs match {
      case h::t => {
        val file = new File(path + h._1 + "/" + h._2)
        if (file.isDirectory)
          loop(path,
               Nil ++ (Nil ++ file.list).map(x => (h._1 + "/" + h._2,x)) ++ t,
               delSuccess,
               {bool => {
                 val success = file.delete
                 stack(bool && success)
               }}
          )
        else {
          val success = file.delete
          loop(path, t, delSuccess && success, stack)
        }
      }
      case Nil => stack(delSuccess)
    }

    val file = (new File(path))
    if (file.isDirectory) {
      val parent = {
        val p = file.getParent
        (if (p == null) ""
        else p + "/")
      }
      val ok = loop(parent,(file.getName,"")::Nil)
      if (!ok) throw new Exception("Could not delete everything in directory [" + path + "].")
      else ()
    } else {
      val ok = file.delete
      if (!ok) throw new Exception("Could not delete file [" + path + "].")
      else ()
    }
  }

  /** Extracts all the lines from a file.
   * @param file the file
   * @return the lines of the input file.
   */
  def fileToList(file: String) = {
    val reader = new BufferedReader(new FileReader(file))
    def loop(res: List[String] = Nil): List[String] = {
      val line = reader.readLine()
      if (line == null) res.reverse
      else loop(line::res)
    }
    loop()
  }

  /** Creates a new directory.
   * Will also create any non existent intermediary directory.
   * @param path the path of the folder to create.
   */
  def mkdirs(path: String) = (new File(path)).mkdirs()

  /** Writes some lines in a file.
   * '''The file does not have to exist, but the folder it is / will be in does.'''
   * @param target the path to the target file.
   * @param lines the lines to write.
   * @param append if <code>true</code>, content will be appended to the target file.
   *   Will overwrite the content (if any) if <code>false</code> (default).
   */
  def linesToFile(target: String,
                  lines: List[String],
                  append: Boolean = false) = {
    val targetFile = new File(target)
    
    if (!targetFile.isDirectory) {
      val bw = new BufferedWriter(new FileWriter(targetFile,append))
      for (line <- lines) bw.write(line + "\n")
      bw.flush
      bw.close
    } else
      throw new Exception("Illegal path " + target + " denotes a directory.")
  }

  /** Writes a <code>String</code> in a file.
   * '''The file does not have to exist, but the folder it is / will be in does.'''
   * @param target the path to the target file.
   * @param text the <code>String</code> to write.
   * @param append if <code>true</code>, content will be appended to the target file.
   *   Will overwrite the content (if any) if <code>false</code> (default).
   */
  def textToFile(target: String,
                 text: String,
                 append: Boolean = false) = {
    val targetFile = new File(target)
    if (!targetFile.isDirectory) {
      val bw = new BufferedWriter(new FileWriter(targetFile,append))
      bw.write(text)
      bw.flush
      bw.close
    } else
      throw new Exception("Illegal path " + target + " denotes a directory.")
  }

  /** Default argument for <code>copyFile</code>. */
  protected object SimpleCopy {
    def reactToLine(line: Option[String],
                    writer: Writer): Unit = line match {
      case Some(l) =>
        writer.write(l + "\n")
      case None => ()
    }
  }

  /** Copies a file with possible alteration of its lines.
   * @param sourcePath the path to the source file.
   * @param targetPath the path to the target file.
   * @param append if <code>true</code>, content will be appended to the target file.
   *   Will overwrite the content (if any) if <code>false</code> (default).
   * @param lineChanger an object with a <code>reactToLine</code> method which takes a <code>Option[String]</code>
   *   (<code>Some</code> of the current line of the source file or None if <code>EOF</code> was reached) and a
   *   <code>Writer</code> (writing in the target file).
   *   '''Do not forget to add <code>\\n</code> at the end of line if you want a new line.'''
   *   Default value simply performs <code>writer.write(line + "\\n")</code>.
   */
  def copyFile(sourcePath: String,
               targetPath: String,
               append: Boolean = false,
               lineChanger: {def reactToLine(line: Option[String],writer: Writer): Unit} = SimpleCopy): Unit = {
    val source = new File(sourcePath)
    val target = new File(targetPath)
    if (!source.isDirectory && !target.isDirectory) {
      val br = new BufferedReader(new FileReader(source))
      val bw = new BufferedWriter(new FileWriter(target,append))
      loop()

      def loop(): Unit = {
        val line = br.readLine
        if (line != null) {
          lineChanger.reactToLine(Some(line),bw)
          loop()
        } else {
          lineChanger.reactToLine(None,bw)
          bw.flush
          bw.close
        }
      }
    } else
      if (!source.isDirectory)
        throw new Exception("Illegal source path: " + sourcePath + " denotes a directory, not a file.")
      else
        throw new Exception("Illegal target path: " + targetPath + " denotes a directory, not a file.")
  }

}
