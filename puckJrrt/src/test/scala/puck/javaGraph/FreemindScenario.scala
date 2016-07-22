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

package puck.javaGraph

import java.io.File

import org.extendj.ast.{HasNode, JavaJastAddDG2AST}
import puck.{NonExistentEdge, Project}
import puck.config.ConfigParser
import puck.graph._
import puck.graph.transformations.rules.Redirection
import puck.jastadd.ExtendJGraphUtils.Rules._
import puck.javaGraph.nodeKind.Interface
import puck.util.LoggedEither._
import puck.util.PuckFileLogger

import scalaz.std.list._

/**
  * Created by Loïc Girault on 22/06/16.
  */
object FreemindScenario {

  def inferUsage(g : DependencyGraph,
                 users : Seq[NodeId],
                 typeUsed : NodeId) : Set[NodeId] = {
    val typeMembersUses = users.foldLeft(Set[NodeIdP]()){
      (acc, user) =>
        acc ++ (g.usedBy(user) filter (g.contains(typeUsed,_)) map ((user,_)))
    }

    typeMembersUses.foldLeft(Set[(NodeIdP, NodeIdP)]()){
      (acc, tmu) =>
        acc ++ Redirection.cl(g, tmu)
    } map (_._2._2)

  }

  def printUsageFullLocation(dG2AST: JavaJastAddDG2AST, g : DependencyGraph, use : NodeIdP) : Unit = {
    val (user, _) = use
    dG2AST.graph2ASTMap(g.container_!(user)) match {
      case hn: HasNode =>
        val n = hn.node
        println(n.fullLocation())
      case _ => ()
    }
  }
  import puck.graph.ShowDG._
  def checkNonEmptyQualifyingRelationship(dG2AST: JavaJastAddDG2AST, g : DependencyGraph) = {
    println("****************************************************************")
    println("****************************************************************")
    println("****************************************************************")
    println("[CHECK non empty qualifying relationship ]")
    g.usesListExludingTypeUses.foreach {
      tmu =>
        try {
          if (g.kindType(tmu._2) == InstanceValueDecl
              &&  g.typeUsesOf(tmu).isEmpty) {

            printUsageFullLocation(dG2AST, g, tmu)
            println((g, tmu).shows + " has no type uses !!!")
          }
        } catch {
//          case NonExistentEdge(u) if u.used == DependencyGraph.findElementByName(g, "java.lang.Object[]").get.id => ()
//          case NonExistentEdge(u) if u.used == DependencyGraph.findElementByName(g, "java.lang.String[]").get.id => ()
//          case NonExistentEdge(u) if g.fullName(u.used).endsWith("[]") => ()
          case NonExistentEdge(u) =>
            printUsageFullLocation(dG2AST, g, tmu)
            println((g, u).shows + " type uses of " +(g, tmu).shows + " does not exist ")
        }
    }
    println("[CHECK ended ]")
    println("****************************************************************")
    println("****************************************************************")
  }

  def main(args : Array[String]) : Unit = {
    val cfgFilePath = "/home/lorilan/projects/constraintsSolver/freemind-puck-cfg.xml"
    val planPath = "/home/lorilan/test_cases_for_puck/QualitasCorpus/Systems/freemind/freemind-1.0.1/src/freemind/00fremind_plan.pck"
    val project = new Project(ConfigParser(new File(cfgFilePath)),JavaJastAddDG2AST)

    implicit val logger = new PuckFileLogger(_ => true, project.logFile.get)

    val s = new ScenarioFactory(project.loadGraph().asInstanceOf[JavaJastAddDG2AST])

    val constraints = project.parseConstraints(s.dg2ast).get

    import puck.graph.ConstraintsOps

    import s._

    println((graph, constraints).violations().size + " violations")
    val ctrlName = "freemind.controller.Controller"
    val controller : NodeId = fullName2id(ctrlName)

    val errorMsgMethod : NodeId = s"$ctrlName.errorMessage(Object)"
    val infoMsgMethod : NodeId = s"$ctrlName.informationMessage(Object)"

    def redirectWrongUsers(g : DependencyGraph, wronglyUsed : NodeId, abs : Abstraction) =
      (g,constraints).wrongUsers(wronglyUsed).foldLoggedEither(g){
        (g, user) =>
          if( g.uses(user, wronglyUsed) )
            redirection.redirectUsesAndPropagate(g, (user, wronglyUsed), abs)
          else LoggedSuccess(g) // redirected in a previous iteration of the fold loop
      }

      import puck.graph.ShowDG._/*++
      (graph,constraints).wrongUsers(infoMsgMethod)*/
    (graph,constraints).wrongUsers(errorMsgMethod) foreach {
      println("(graph,constraints).wrongUsers(errorMsgMethod)")
      m =>
        (graph, m).println(desambiguatedFullName)
        println("typeUsesOf.isEmpty = " + graph.typeUsesOf((m,errorMsgMethod)).isEmpty)

    }

    val methods =
      inferUsage(graph,
        (graph,constraints).wrongUsers(errorMsgMethod) ++
        (graph,constraints).wrongUsers(infoMsgMethod),
        controller)


    //checkNonEmptyQualifyingRelationship(s.dg2ast, graph)

    println("-----------")

    val ltg = for {
     absg2 <- abstracter.abstractTypeDeclAndReplaceByAbstractionWherePossible(graph.mileStone,
        graph getConcreteNode controller, Interface,
        SupertypeAbstraction,
        methods.toList map graph.getConcreteNode)

     (abs, g2) =  absg2

      AccessAbstraction(itId, _) = abs

      errorMsgAbs = g2.abstractions(errorMsgMethod).head

      infoMsgAbs = g2.abstractions(infoMsgMethod).head

      g3 = g2.addContains("freemind.modes", itId)
              .setName(itId, "Messenger")

      g4 <- redirectWrongUsers(g3, errorMsgMethod, errorMsgAbs)

      g5 <- redirectWrongUsers(g4, infoMsgMethod, infoMsgAbs)

      g6 <- move.staticDecl(g5, "freemind.controller.filter.util", "freemind.modes")

      g7 <- move.typeMember(g6,
        List[NodeId]("freemind.modes.mindmapmode.MindMapArrowLinkModel.changeInclination(MapView,int,int,int,int)"),
        "freemind.view.mindmapview.MapView")
      g8 <- remove.concreteNode(g7, "freemind.modes.browsemode.BrowseArrowLinkModel.changeInclination(MapView,int,int,int,int)")

    } yield g7


    ltg match {
      case LoggedSuccess(_, g) =>
        println("success : " + (g, constraints).violations().size + " violations")
        Recording.write(planPath, dg2ast.nodesByName, g)
        project.outDirectory foreach {
          dir =>
            println("printing code in " + dir.getAbsolutePath)
            s.applyChanges(g, dir)
        }
      case LoggedError(log, _) =>
        println("error : "+ log)
    }

  }

}
