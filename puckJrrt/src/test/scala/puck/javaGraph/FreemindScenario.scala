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

import org.extendj.ast.JavaJastAddDG2AST
import puck.{NonExistentEdge, Project, PuckError}
import puck.config.ConfigParser
import puck.graph.{Abstraction, AccessAbstraction, DependencyGraph, InstanceValueDecl, LoggedError, LoggedSuccess, NodeId, NodeIdP}
import puck.graph.constraints.SupertypeAbstraction
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


  import puck.graph.ShowDG._
  def checkNonEmptyQualifyingRelationship(g : DependencyGraph) = {
    println("****************************************************************")
    println("****************************************************************")
    println("****************************************************************")
    println("[CHECK non empty qualifying relationship ]")
    g.usesListExludingTypeUses.foreach {
      tmu =>
        try {
          if (g.kindType(tmu._2) == InstanceValueDecl
              &&  g.typeUsesOf(tmu).isEmpty)
            println((g, tmu).shows + " has no type uses !!!")
        } catch {
          case NonExistentEdge(u) if u.used == DependencyGraph.findElementByName(g, "java.lang.Object[]").get.id => ()
          case NonExistentEdge(u) if u.used == DependencyGraph.findElementByName(g, "java.lang.String[]").get.id => ()
          case NonExistentEdge(u) =>
          throw new PuckError("!!!! " + (g, u.user).shows(desambiguatedFullName) + " uses " + (g, u.used).shows(desambiguatedFullName) + " does not exist !!")
        }
    }
    println("[CHECK ended ]")
    println("****************************************************************")
    println("****************************************************************")
  }

  def main(args : Array[String]) : Unit = {
    val cfgFilePath = "/home/lorilan/projects/constraintsSolver/freemind-puck-cfg.xml"
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
        (g, user) => redirection.redirectUsesAndPropagate(g, (user, wronglyUsed), abs)
      }

    import puck.graph.ShowDG._
    println("(graph,constraints).wrongUsers(errorMsgMethod)")
    (graph,constraints).wrongUsers(errorMsgMethod) ++
      (graph,constraints).wrongUsers(infoMsgMethod) foreach {
      m =>
        (graph, m).println(desambiguatedFullName)

    }

    val methods =
      inferUsage(graph,
        (graph,constraints).wrongUsers(errorMsgMethod) ++
        (graph,constraints).wrongUsers(infoMsgMethod),
        controller)


    checkNonEmptyQualifyingRelationship(graph)

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

      g3 = g2.setName(itId, "Messenger")
              .addContains("freemind.modes", itId)

      g4 <- redirectWrongUsers(g3, errorMsgMethod, errorMsgAbs)

//      g5 <- redirectWrongUsers(g4, infoMsgMethod, infoMsgAbs)

    } yield {
        g4
    }

    ltg match {
      case LoggedSuccess(_, g) =>
        println("success : " + (g, constraints).violations().size + " violations")
      case LoggedError(log, _) =>
        println("error : "+ log)
    }

  }

}
