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
import puck.graph.transformations.{MutabilitySet, Recording}
import MutabilitySet.MutabilitySetOps
import puck.graph.transformations.rules.CreateTypeMember
import puck.jastadd.ExtendJGraphUtils.Rules
import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.jastadd.JavaProject
import puck.javaGraph.nodeKind.{Field, Method}
import puck.util.{PuckFileLogger, PuckLogger, PuckSystemLogger}
import ShowDG._
import puck.graph.ConstraintsOps

import scala.util.Random

object MutantGen {

  def legalMoveCandidate(g : DependencyGraph, id : NodeId, mutabilitySet : MutabilitySet.T) = {

    lazy val hostType = g container_! id

    def concrete : Boolean = g.definitionOf(id).nonEmpty
    def overridesOrImplements : Boolean =
      g abstractions id exists {
        case AccessAbstraction(_, SupertypeAbstraction) => true
        case _ => false
      }

    def isOverridenOrImplemented : Boolean = {
      val subTypes = g.subTypes(hostType)

      subTypes exists {
        st => g.content(st).exists{
          subM => g.abstractions(subM) contains AccessAbstraction(id, SupertypeAbstraction)
        }
      }
    }

    def superUses : Boolean = {
      val defId = g.definitionOf_!(id)
      try {
        g.usedBy(defId) exists {
          used => g.typeUsesOf((defId, used)).exists {
            case (typeUser, typeUsed) => typeUser == hostType && g.isa(hostType, typeUsed)
          }
        }
      } catch {
        case err @ NonExistentEdge(edge) =>
          import ShowDG._
          println( (g, edge).shows + " does not exist")
          false

      }
    }

    val n = g.getConcreteNode(id)

    def hasParameters : Boolean = n.kind match {
      case Method =>
        g.content(id) exists(cid => g.kindType(cid) == Parameter)
      case _ => false
    }


    if (mutabilitySet.isMutable(n.id) && (n.kind.kindType  match {
      case TypeDecl  | StaticValueDecl => true
      case InstanceValueDecl => hasParameters &&
        ! overridesOrImplements &&
        ! isOverridenOrImplemented &&
        concrete && !superUses
      case _ => false
    })) Some(n)
    else None
  }


  def makeRandomMove(num : Int, g : DependencyGraph,
                     forbiddenDependency : Int,
                     cm : ConstraintsMaps)
                    (implicit mutantLogger : PuckLogger,
                     mutabilitySet: MutabilitySet.T): DependencyGraph  =
    if (num == 0 ) g
    else {
      var candidate = legalMoveCandidate(g,  Random.nextInt(g.numNodes), mutabilitySet)
      while(candidate.isEmpty)
        candidate = legalMoveCandidate(g,  Random.nextInt(g.numNodes), mutabilitySet)

      val n = candidate.get

      val (ltg, containerId) : (LoggedTG, NodeId) = n.kind.kindType match {
        case TypeDecl | StaticValueDecl =>
          // /!\ typeDecl not necessary static
          val candidateList =  g.nodes.filter(g.canContain(_, n)).toArray
          if(candidateList.isEmpty)
            (LoggedError(candidateList.length + " candidates"), -1)
          else {
            val containerCandidate = candidateList(Random.nextInt(candidateList.length))

            (Rules.move.staticDecl(g, n.id, containerCandidate.id), containerCandidate.id)
          }
        case InstanceValueDecl =>
          val p = g.content(n.id).filter(g.kindType(_) == Parameter)
          val candidateList0 =
            (p map (pid => Type.mainId(g.typ(pid))) filter ( tid =>
              g.canContain(g.getConcreteNode(tid), n)
              )).toArray

          val candidateList =
            if(candidateList0.nonEmpty) candidateList0
            else g.nodes.filter(g.canContain(_, n)).map(_.id).toArray

          val containerCandidate = candidateList(Random.nextInt(candidateList.length))

          (Rules.move.typeMember(g, List(n.id), containerCandidate, Some(CreateTypeMember(Field))),
            containerCandidate)
        case _ => puck.error()
      }

      ltg match {
        case LoggedSuccess((_,g1)) =>

          val fd = (g1, cm).violations.size
          if(fd > forbiddenDependency) {
            mutantLogger writeln ("num = " + num)
            mutantLogger writeln ("move candidate = " + (g, n.id).shows(desambiguatedFullName))
            mutantLogger writeln ("container candidate = " + (g, containerId).shows(desambiguatedFullName))
            makeRandomMove(num - 1, g1.mileStone, fd, cm)
          }
          else
            makeRandomMove(num, g, forbiddenDependency, cm)
        case _ => makeRandomMove(num, g, forbiddenDependency, cm)
      }
    }

  def main (args: Array[String]) : Unit = {

    implicit val logger = new PuckSystemLogger(_ => true)

    val root = "/home/lorilan/projects/arianne-marauroa"

    val projectFileName = root + "/original-puck-cfg.xml"

    val p = JavaProject.withConfig(projectFileName)

    val dg2ast : JavaJastAddDG2AST = p.loadGraph().asInstanceOf[JavaJastAddDG2AST]

    val scm = p.parseConstraints(dg2ast)

    val filePrefix = "mutant-01move-03"
    import puck.util.FileHelper.FileOps
    val recFile = p.workspace \ s"$filePrefix.pck"
    val logFile = p.workspace \ s"$filePrefix.log"

    val numberOfmove = 1

    import dg2ast._

    val mutantLogger = new PuckFileLogger(_=> true, logFile)

    scm foreach {
      cm =>
        val mutant = makeRandomMove(numberOfmove, initialGraph,
          (initialGraph, cm).violations.size, cm)(mutantLogger, dg2ast.initialMutability)
        mutantLogger.writeln((mutant,cm).violations.size + " violations")

        Recording.write(recFile.getAbsolutePath, dg2ast.nodesByName, mutant)
        dg2ast(mutant)
        p.outDirectory foreach dg2ast.printCode
    }

  }
}
