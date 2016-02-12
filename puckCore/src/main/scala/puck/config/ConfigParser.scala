package puck.config

import java.io.{File, FileWriter}

import puck.util.HMap.HMapKey
import puck.util.{HMap, XMLUtils}

import scala.xml.XML
/**
  * Created by lorilan on 08/02/16.
  */

object Config {

  abstract class ConfigKey[Phantom : Manifest](v : String)
    extends HMapKey[String, Phantom](v) {

    def tagedString(s : String,
                    attributes : List[(String, String)] = List()) : String = {
      val attrString =
        if(attributes.isEmpty) ""
        else
          attributes map {case (k, v) => s"""$k="$v""""} mkString (" ", " ", "")
      s"<$v$attrString>$s</$v>"
    }


    def xmlValue(config: Config) : String
  }

  type Config = HMap[ConfigKey]

  def empty: Config = HMap.empty


  sealed abstract class FileFinder {

    import puck.util.FileHelper.FileOps

    val path : String
    def resolvePath(workspace : File) : String =
      if(path startsWith "/") path
      else (workspace \ path).getAbsolutePath
  }

  case class SingleFile(path : String) extends FileFinder
  case class Root(path : String, suffix : String) extends FileFinder

  class FileKey(v : String) extends ConfigKey[SingleFile](v) {
    def xmlValue(config: Config) : String =
      config get this match {
        case Some(SingleFile(path)) => s"\t${tagedString(path)}\n"
        case None => ""
      }
  }

  class FileListKey(v : String) extends ConfigKey[List[FileFinder]](v) {

    def xmlValue(ff: FileFinder) : String = ff match {
      case SingleFile(path) => tagedString(path)
      case Root(path, suffix) => tagedString(path, List(("rec", suffix)))
    }

    def xmlValue(config: Config) : String =
      config get this match {
        case Some(l) =>
          l map xmlValue mkString ("\t", "\n\t", "\n")
        case None => ""
      }
  }

  object Keys {
    implicit def str2FileKey(v : String)  : FileKey = new FileKey(v)
    implicit def str2FileListKey(v : String)  : FileListKey = new FileListKey(v)

    val workspace : FileKey = "workspace"

    val srcs : FileListKey = "src"
    val sourcepaths : FileListKey = "sourcepath"
    val classpath : FileListKey = "classpath"
    val bootclasspath : FileListKey = "bootclasspath"

    val out : FileKey = "out"
    val decouple : FileKey = "decouple"
    val log : FileKey =  "log"

    val dotPath : FileKey =  "dot-path"
    val editor : FileKey =  "editor"

  }

  val singleValueKeys : List[FileKey] = {
    import Keys._
    List(out, decouple, log)
  }
  val listValueKeys : List[FileListKey] = {
    import Keys._
    List(srcs, sourcepaths, classpath, bootclasspath)
  }

  val defautlConfig =
    (empty
      put (Keys.workspace, SingleFile("."))
      put (Keys.srcs, List(Root("src", ".java")))
      put (Keys.classpath, List(Root("lib", ".jar")))
      put (Keys.out, SingleFile("out"))
      put (Keys.decouple, SingleFile("decouple.wld"))
      put (Keys.log, SingleFile("puck-log.txt"))
      )

  val defaultFileName : String = "puck.xml"

  import puck.util.FileHelper.FileOps
  def defaultConfFile(workspace : File) : File =
    workspace \ Config.defaultFileName
}
import Config._
object ConfigParser {

  import XMLUtils._

//  def createDefault(configFile : File) : Config = {
//      val f = new FileWriter(configFile)
//      f write
//        s"""<puck-config>
//          |    <workspace>.</workspace>
//          |    <!-- root of source where all .java will be searched -->
//          |    <src rec=".java">${Default.srcRoot}</src>
//          |    <!-- root of lib where all .jar will be searched -->
//          |    <classpath rec=".jar">${Default.classpathRoot}</classpath>
//          |    <out>${Default.out}</out>
//          |    <decouple>${Default.decouple}</decouple>
//          |    <log>${Default.log}</log>
//          | </puck-config>""".stripMargin
//
//      f.close()
//      this.apply(configFile)
//  }




  def apply(config : File) : Config = {
    val n = XML.loadFile(config)

    val root =
      (n \ Keys.workspace).textOption getOrElse config.getParent

    val initConf = Config.empty put (Keys.workspace, SingleFile(root))

    val c1 = Config.singleValueKeys.foldLeft(initConf){
      case (c, k) =>
        (n \ k).textOption map {
          v => c put (k, SingleFile(v))
        } getOrElse c
    }
    Config.listValueKeys.foldLeft(c1){
      case (c, k) =>
        (n \ k).foldLeft(c) {
          (c1, knode) =>
            val prev = c1 getOrElse (k, List())
            val path = knode.text
            val newVal =
              knode singleOptionAttribute "rec" match {
                case None =>  SingleFile(path) :: prev
                case Some(suffix) => Root(path, suffix) :: prev
              }
            c1 put (k, newVal)

        }
    }
  }
}

object ConfigWriter {

  private def write(appendable: Appendable, conf : Config) : Unit = {
    appendable.append("<puck-config>\n")
    conf.keys foreach {
      k => appendable.append(k xmlValue conf)
    }
    appendable.append("</puck-config>")
  }

  def apply(configFile : File, conf : Config) : Unit ={
    val fw = new FileWriter(configFile)
    write(fw, conf)
    fw.close()
  }


  def show(conf : Config) : String = {
    val sb = new StringBuffer
    write(sb, conf)
    sb.toString
  }
}