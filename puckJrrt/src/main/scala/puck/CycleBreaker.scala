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

import org.extendj.ast.JavaJastAddDG2AST
import puck.jastadd._
import puck.javaGraph.{JavaDotHelper, JGraphUtils}
import puck.util.PuckSystemLogger

/**
  * Created by Loïc Girault on 10/12/15.
  */
object CycleBreaker {
  def main(args: Array[String]) = {
    implicit val logger = new PuckSystemLogger(_ => true)

    val gu = new JGraphUtils {
      val dg2astBuilder: DG2ASTBuilder = JavaJastAddDG2AST
    }

    val dg2ast = if(args.isEmpty) {
      val fh = JavaProject()
      fh.loadGraph().asInstanceOf[JavaJastAddDG2AST]
    }
    else JavaJastAddDG2AST.fromFiles(args.toList, List(), List(), List())

    val n = dg2ast.nodesByName("screen.WelcomeCapital.printCapital(String)")

    QuickFrame(dg2ast.initialGraph, "g", JavaDotHelper)

//    val searchControlStrategy =
//          new CouplingConstraintSolvingControl(
//            gu.transformationRules, dg2ast.initialGraph,
//            dg2ast.initialGraph getConcreteNode n)
//
//    val engine =
//         new SearchEngine(new DepthFirstSearchStrategy(), searchControlStrategy)
//
//        engine.explore()
//    val g2 = Forbidener.genConstraints(dg2ast.initialGraph)
//    println("#####################")
//    println("Generated constraints :")
//
//    g2.printConstraints(logger, PuckLog.defaultVerbosity)
//
//    import ShowDG._
//    println("#####################")
//
//    val initialViolations = g2.violations()
//    println("Violations :")
//    initialViolations.foreach{
//      e =>
//      dg2ast.graph2ASTMap get e.user match {
//        case Some(n : HasNode) =>
//          print(n.node.compilationUnit().pathName() + ", l "+ n.node.location()+" ")
//        case _ => ()
//      }
//
//      (g2,e).println(fullNameEdgeCord)
//    }



//    val searchControlStrategy =
//      new CouplingConstraintSolvingAllViolationsControl(
//        JGraphUtils.transformationRules, g2,
//        JGraphUtils.violationsKindPriority)
//
//    val engine =
//      new SearchEngine(new DepthFirstSearchStrategy(),
//        searchControlStrategy, Some(1))
//
//    engine.explore()
//
//
//    if(engine.successes.isEmpty)
//      println("no solution found")
//    else {
//      val res =engine.successes.head.loggedResult
//      res match {
//        case LoggedEither(_, \/-(g3)) =>
//          println("applying result")
//          dg2ast.apply(g3)
//        case _ => println("error, should have been a success")
//      }
//    }
  }


}
