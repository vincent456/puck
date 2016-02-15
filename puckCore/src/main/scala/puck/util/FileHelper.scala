package puck.util

import java.io.{FileReader, BufferedReader, File}
import java.util.regex.{Matcher, Pattern}
import scala.language.implicitConversions

object FileHelper {

  def removeEquals[T](l1 : List[T], l2 : List[T]) : (List[T], List[T]) =
    (l1, l2) match {
      case (h1 :: t1, h2 :: t2) if h1 == h2 => removeEquals(t1, t2)
      case _ => (l1, l2)
    }


  implicit class FileOps(val f : File) extends AnyVal {
    def \(child : String) : File =
      new File(f.getAbsolutePath + File.separator + child)

    def ancestors : List[File] = {

      def aux(acc : List[File], f: File) : List[File] = {
        val p = f.getParentFile
        if(p == null) acc
        else aux(p :: acc, p)
      }

      aux(List(), f)
    }

    def pathRelativeTo(other : File) : String = {

      val thisAncestors = f.ancestors ::: List(f)
      val otherAncestors = other.ancestors ::: List(other)

      removeEquals(thisAncestors, otherAncestors) match {
        case (these, Nil) =>
          these map (_.getName) mkString File.separator
        case _ => f.getPath
      }
    }
  }

//  object FileOption {
//    implicit def fileOptionToOptionFile(fo : FileOption) : Option[File] =
//      fo.get
//  }
//
//  class FileOption(private [this] var sf : Option[File] = None) {
//
//    override def toString = s"FileOption(${sf.toString})"
//
//    def this(f : File) = this(Some(f))
//
//    def get = sf
//    def ! = sf.get
//    def set(sf : Option[File]) =
//      sf match {
//        case None => ()
//        case Some(f) => val fc = f.getCanonicalFile
//          this.sf =
//            if(fc.exists()) Some(fc)
//            else None
//      }
//
//    def toOption = sf
//  }

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
                   ignoredSubDir : Option[String]) : List[String] =
    findAllFiles(suffix, ignoredSubDir, List(), root)

  def findAllFiles(suffix : String,
                   ignoredSubDir : Option[String],
                   res: List[String],
                   f: File) : List[String] = {
    
    val ignore : String => Boolean =
    ignoredSubDir match {
      case None => _ => false
      case Some(subDir) => _ == subDir
    }
    
    def aux(res: List[String],
            f: File) : List[String] = {
      if (f.isDirectory)
        f.listFiles().foldLeft(res){
          case (l, f0) =>
            if(ignore(f0.getName)) l
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
