/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.config
import puck._
import java.io.{File, FileWriter}

import puck.util.HMap.HMapKey
import puck.util.{HMap, XMLUtils}

import scala.xml.XML
/**
  * Created by Loïc Girault on 08/02/16.
  */

object Config {

  def taggedString(tag : String, content : String,
                   attributes : List[(String, String)] = List()) : String = {
    val attrString =
      if(attributes.isEmpty) ""
      else
        attributes map {case (k, v) => s"""$k="$v""""} mkString (" ", " ", "")
    s"<$tag$attrString>$content</$tag>"
  }

  abstract class ConfigKey[Phantom : Manifest](v : String)
    extends HMapKey[String, Phantom](v) {

    def taggedString(s : String, attributes : List[(String, String)] = List()) : String =
      Config.taggedString(v, s, attributes)


    def xmlValue(config: Config) : String
  }

  type Config = HMap[ConfigKey]

  def empty: Config = HMap.empty


  sealed abstract class FileFinder {

    import puck.util.FileHelper.FileOps

    val path : String
    def resolvePath(workspace : File) : String =
      if(new File(path).isAbsolute) path
      else (workspace \ path).getAbsolutePath
  }

  case class SingleFile(path : String) extends FileFinder
  case class Root(path : String, suffix : String, exclude : Seq[String]) extends FileFinder

  class FileKey(v : String) extends ConfigKey[SingleFile](v) {
    def xmlValue(config: Config) : String =
      config get this match {
        case Some(SingleFile(path)) => s"\t${taggedString(path)}\n"
        case None => ""
      }
  }

  class FileListKey(v : String) extends ConfigKey[List[FileFinder]](v) {

    def xmlValue(ff: FileFinder) : String = ff match {
      case SingleFile(path) => taggedString(path)
      case Root(path, suffix, Seq()) => taggedString(path, List(("rec", suffix)))
      case Root(path, suffix, s) =>
        val content = s map (Config.taggedString("exclude",_))
        taggedString(content mkString "\n", List(("rec", suffix), ("root", path)))
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

  def defautlConfig(workspace : File) =
    (empty
      put (Keys.workspace, SingleFile(workspace.getAbsolutePath))
      put (Keys.srcs, List(Root("src", ".java", Seq())))
      put (Keys.classpath, List(Root("lib", ".jar", Seq())))
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
            val newVal =
              (knode singleOptionAttribute "rec",
                knode singleOptionAttribute "root") match {
                case (None,_) =>
                  val path = knode.text
                  SingleFile(path) :: prev
                case (Some(suffix), None) =>
                  val path = knode.text
                  Root(path, suffix, Seq()) :: prev
                case (Some(suffix), Some(path)) =>
                  val ex = (knode \ "exclude").theSeq map (_.text)
                  Root(path, suffix, ex) :: prev
              }
            c1 put (k, newVal)

        }
    }
  }
}

object ConfigWriter {

  private def write(appendable: Appendable, conf : Config) : Unit = {
    appendable append "<puck-config>\n"
    conf.keys foreach {
      k => appendable append (k xmlValue conf)
    }
    ignore( appendable append "</puck-config>" )
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