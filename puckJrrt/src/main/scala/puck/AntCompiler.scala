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

package puck

import java.io.File

import org.apache.tools.ant.taskdefs.Javac
import org.apache.tools.ant.taskdefs.compilers.CompilerAdapter
import org.apache.tools.ant.types.Path
import puck.config.{ConfigWriter, Config}
import puck.config.Config._

import scala.sys.process._

/**
  * Created by Loïc Girault on 11/03/16.
  */
class AntCompiler extends CompilerAdapter {

  var cfgFile : File = _
  def execute(): Boolean = {

    val sep = sys.props("file.separator")
    val classPath = sys.props("java.class.path")
    val path = sys.props("java.home") + sep + "bin" + sep + "java"


    //val cfgFilePath = cfgFile.getPath.replaceAllLiterally(" ", "\\ ")
    Seq(path, "-cp", classPath, "puck.Front", cfgFile.toString).!

    false
  }


  def addPotentiallyNullOption(cfg : Config, k : FileListKey, p : Path) : Config =
    Option(p).map(_.list().toList.map(f => SingleFile(f))).foldLeft(cfg){
      (cfg0, fl) =>
        println(s"putting $k $fl")
        cfg0 put (k, fl)
    }

  def setJavac(attributes: Javac): Unit = {
    println("getBootclasspath = " + attributes.getBootclasspath)
    println("getClasspath = " + attributes.getClasspath)
    println("getSourcepath = " + attributes.getSourcepath)
    println("getFileList = " +  attributes.getFileList)

    //import scala.collection.JavaConverters._
    val srcs = attributes.getFileList.toList.map(f => SingleFile(f.getPath))
    val classPath = attributes.getClasspath.list().toList.map(f => SingleFile(f))



    val cfg = (Config.empty put(Keys.srcs, srcs)
                            put(Keys.classpath, classPath)
                            put(Keys.workspace, SingleFile(attributes.getProject.getBaseDir.getPath)))


    val cfg1 = addPotentiallyNullOption(cfg, Keys.sourcepaths, attributes.getSourcepath)
    val cfg2 = addPotentiallyNullOption(cfg1, Keys.bootclasspath, attributes.getBootclasspath)

    cfgFile = Config.defaultConfFile(attributes.getProject.getBaseDir)

    ConfigWriter(cfgFile, cfg2)
  }
}
