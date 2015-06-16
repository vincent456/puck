package puck.util

import java.io.{FileReader, BufferedReader, File}
import java.util.regex.{Matcher, Pattern}
import scala.language.implicitConversions

/**
 * Created by lorilan on 11/08/14.
 */
object FileHelper {

  implicit def string2file(filePath : String) : File = new File(filePath)

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

//  def initStringLiteralsMap(file: File): java.util.Map[String, java.util.Collection[AST.BodyDecl]] =
//  if(!file.exists()) new java.util.HashMap()
//  else {
//    val reader: BufferedReader = new BufferedReader(new FileReader(file))
//
//    val pat = Pattern.compile(Pattern.quote("string('") + "([^']*)" +
//      Pattern.quote("')"))
//
//    val smap = new java.util.HashMap[String, java.util.Collection[AST.BodyDecl]]()
//
//    def read() : Unit = {
//      val line = reader.readLine
//      if (line != null) {
//        val m: Matcher = pat.matcher(line)
//        while (m.find) {
//          smap.put(m.group(1), new java.util.ArrayList[AST.BodyDecl])
//        }
//        read()
//      }
//    }
//    read()
//    reader.close()
//    smap
//  }

  def findAllFiles(root : File,
                   suffix : String,
                   ignore: String) : List[String] =
    findAllFiles(suffix, ignore, List(), root)

  def findAllFiles(suffix : String,
                   ignore: String,
                   res: List[String],
                    f: File) : List[String] = {
    def aux(res: List[String],
            f: File) : List[String] = {
      if (f.isDirectory)
        f.listFiles().foldLeft(res){
          case (l, f0) =>
            if(f0.getName == ignore) l
            else aux(l, f0)
        }
      else {
        if (f.getName.endsWith(suffix))
          f.getPath :: res
        else
          res
      }
    }

    aux(res, f)

  }
  
}
