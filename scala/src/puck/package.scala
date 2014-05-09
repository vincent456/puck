import java.io._
import java.nio.charset.Charset
import java.util
import java.util.regex.{Matcher, Pattern}
import scala.language.implicitConversions
/**
 * Created by lorilan on 08/05/14.
 */
package object puck {

  implicit def string2file(filePath : String) = new File(filePath)

  def fileLines(file: File, keepEmptyLines: Boolean = false): List[String] = {
    if(!file.exists()) return List()

    //val fis: InputStream = new FileInputStream(file.getCanonicalFile)
    //val reader: BufferedReader = new BufferedReader(new InputStreamReader(fis, Charset.forName("UTF-8")))
    val reader: BufferedReader = new BufferedReader(new FileReader(file))

    def read(lines : List[String]) : List[String] = {
      val line = reader.readLine
      if (line == null) lines
      else read(line :: lines)
    }
    val lines = read(List())

    reader.close()

    lines.reverse
  }

  def fileLines(fileName: String, keepEmptyLines: Boolean): List[String] =
    fileLines(new File(fileName), keepEmptyLines)

  def initStringLiteralsMap(file: File): java.util.Map[String, util.Collection[AST.BodyDecl]] = {
    val reader: BufferedReader = new BufferedReader(new FileReader(file))

    val pat = Pattern.compile(Pattern.quote("string('") + "([^']*)" +
      Pattern.quote("')"))

    val smap = new util.HashMap[String, util.Collection[AST.BodyDecl]]()

    def read() : Unit = {
      val line = reader.readLine
      if (line != null) {
        val m: Matcher = pat.matcher(line)
        while (m.find) {
          smap.put(m.group(1), new util.ArrayList[AST.BodyDecl])
        }
        read()
      }
    }
    read()
    reader.close()
    smap
  }
}
