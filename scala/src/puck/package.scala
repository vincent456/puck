import java.io._
import java.nio.charset.Charset

/**
 * Created by lorilan on 08/05/14.
 */
package object puck {
  def fileLines(file: File, keepEmptyLines: Boolean = false): List[String] = {
    val fis: InputStream = new FileInputStream(file.getCanonicalFile)
    val reader: BufferedReader = new BufferedReader(new InputStreamReader(fis, Charset.forName("UTF-8")))

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

}
