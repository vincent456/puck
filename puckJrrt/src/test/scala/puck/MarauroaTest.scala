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

import puck.util.{PuckFileLogger, PuckLog, PuckLogger, PuckSystemLogger}
import LoadAndApply._
import puck.config.Config
import puck.config.Config.{Keys, Root, SingleFile}
import puck.graph.transformations.Recording
import puck.graph.{DecoratedGraph, DependencyGraph, LoggedSuccess, Metrics}
import puck.graph.constraints.search.{BlindControl, DecoratedGraphEvaluator}
import puck.search.{AStarSearchStrategy, SearchControl, SearchEngine}
import puck.jastadd.ExtendJGraphUtils._
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

object MarauroaLoadMutant {

  def main(args : Array[String]) : Unit =
    ignore(applyRecords(project("src.original"),
      Seq(root + "/mutant-03moves-01.pck")))

}

object MarauroaMarauroaLoadMutantApplyRec {

  def main(args : Array[String]) : Unit =
    ignore(applyRecursivelyStepByStep(
      project("src.original"),
      root + "/mutant2.pck"))

}

object LoadAndSearchSolutions {


  val baseName = "constraint-gen3-01"

  def main(args : Array[String]) : Unit =
    //SearchSolution(project("src.mutant-03moves-02"))
    SearchSolution(project("src.generated", baseName+".wld"),
      baseName)(new PuckFileLogger(_ => true,
      new File(root + File.separator + baseName + ".log")))
}

object SearchSolution {
  def apply(p : Project, baseName : String)
           (implicit logger : PuckLogger): Unit = {
    val dg2ast = p.loadGraph()

    p.parseConstraints(dg2ast) match {
      case None => logger.writeln("no output constraints")
      case Some(cm) =>
        val f = Metrics.fitness1(_: DependencyGraph, cm, kViols = 1, kComplex = 1).toDouble

        val strategy = new AStarSearchStrategy[DecoratedGraph[Any]](DecoratedGraphEvaluator.equalityByMapping(f))
        val dg = dg2ast.initialGraph.mileStone
        val control = new BlindControl(Rules, dg, cm, violationsKindPriority).
          asInstanceOf[SearchControl[DecoratedGraph[Any]]]

        val engine = new SearchEngine(strategy, control, Some(1))
        logger.writeln("search start !")
        puck.util.Time.time(logger, PuckLog.defaultVerbosity)(engine.explore())


        logger.writeln(engine.successes.size + " solutions found for " + baseName)
        engine.successes map (st => (st.uuid(), st.loggedResult)) foreach {
          case (id, LoggedSuccess(_, (g,_))) =>
            import puck.util.FileHelper.FileOps
            val recFile = p.workspace \  s"$baseName-solution$id.rec"
            Recording.write(recFile.getAbsolutePath, dg2ast.nodesByName, g)
        }
    }
  }
}

object LoadMutantAndSearchSolutions {

  implicit val logger : PuckLogger = new PuckSystemLogger(_ => true)



  def main(args : Array[String]) : Unit = {

    //    val recFile = new File(FrontVars.workspace + "/planB2.puck")
    applyRecords( project("src.original"),
      Seq(root + "/mutant0.pck")).fromOutDir foreach SearchSolution.apply
  }
}


