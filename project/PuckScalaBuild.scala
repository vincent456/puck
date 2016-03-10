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

import sbt._, Keys._

import scalaz._, Scalaz._

object PuckScalaBuild {

  val jarName = settingKey[File]("Jrrt build directory")
  val makejar = taskKey[File]("create the plugin jar")

  def settings = Seq[Setting[_]](
      jarName := baseDirectory.value / "target" / "puck-plugin.jar",
      makejar := {
        val classDir = (classDirectory in Compile).value
        val resDir = (resourceDirectory in Compile).value
        val libDir = (unmanagedBase in Compile).value
        val outFile = jarName.value

        val binFiles = (PathFinder(classDir) ** "*.class").get.toList
        val xmlFile = resDir / "scalac-plugin.xml"
        val libFiles = (PathFinder(libDir) ** "*.jar").get.toList

        val sbin = binFiles map (IO.relativize(classDir, _))
        val sxml = IO.relativize(resDir, xmlFile)
        val slibs = libFiles map (IO.relativize(libDir, _))

        val files = (sbin.sequence, sxml, slibs.sequence) match {
          case (Some(bin), Some(xml), Some(libs)) =>
            (xmlFile :: libFiles ::: binFiles) zip (xml :: libs ::: bin)
          case _ => sys.error(s"files not found")
        }
        IO.jar(files, outFile, new java.util.jar.Manifest())
        outFile
      }
    )
}
