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
 * Author of this file : Loïc Giraul
 */

package puck

import java.io.File

import puck.util.{PuckFileLogger, PuckLogger}
import LoadAndApply._
import puck.config.Config
import puck.config.Config.{Keys, Root, SingleFile}
import puck.graph.constraints.search.{NoVirtualNodes, WithVirtualNodes}
import puck.jastadd.JavaProject

object MarauroaTest {
  implicit val logger : PuckLogger = new PuckFileLogger(_ => true,
    new File("/tmp/pucklog"))

    val root = "/home/lorilan/projects/arianne-marauroa"

  def project(srcFolder : String, constraint : String = "decouple.wld") : Project = {
    val cfg = (Config.empty
      put (Keys.workspace, SingleFile(root))
      put (Keys.srcs, List(Root(srcFolder, ".java", Seq())))
      put (Keys.classpath, List(Root("libs", ".jar", Seq())))

      put (Keys.out, SingleFile("out"))
      put (Keys.decouple, SingleFile(constraint))
      put (Keys.log, SingleFile("puck-log.txt"))
      )
    JavaProject.withConfig(cfg)
  }
}
import MarauroaTest.{logger, root, project}

object MarauroaLoadRecordAndApply {
  val path = "/constraint-gen/1rule/10/solutionWithHeuristic.pck"
  def main(args : Array[String]) : Unit =
    ignore(applyRecords(project("src.original"),
      Seq(root + path )))

}

object MarauroaLoadRecordAndApplyStepByStep {

  def main(args : Array[String]) : Unit =
    ignore(applyRecursivelyStepByStep(
      project("src.original"),
      root + "/constraint-gen1-05-solution-manual-partial.pck"))

}

object LoadAndSearchSolutions {

  val path = "constraint-gen/1rule/08/"

  def main(args : Array[String]) : Unit =
    SearchSolution(project("src.generated", path + "decouple.wld"), path,
      StrategyChoice.DepthFirst, ControlChoice.Heuristic, NoVirtualNodes)
}

//object GenConstraintAndSearchSolutions {
//
//  val numConstraint = 1
//  def genBaseName(id : Int) = s"constraint-gen$numConstraint-$id"
//
//  def main(args : Array[String]) : Unit = {
//    var i = 4
//    while(new File(root + File.separator + genBaseName(i)).exists())
//      i = i + 1
//
//    val baseName = genBaseName(i)
//    val p = project("src.generated", baseName+".wld")
//
//    val (dg, names2id, cm, mutability) =  ConstraintGen(p, baseName, numConstraint)
//    SearchSolution(dg, cm, mutability) map (st => (st.uuid(), st.loggedResult)) foreach {
//          case (id, LoggedSuccess(_, (g,_))) =>
//            import puck.util.FileHelper.FileOps
//            val recFile = p.workspace \  s"$baseName-solution$id.pck"
//            Recording.write(recFile.getAbsolutePath, names2id, g)
//        }
//
//  }
//}




