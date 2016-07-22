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

import org.extendj.ast.JavaJastAddDG2AST
import puck.config.ConfigParser
import puck.graph.transformations.Recording
import puck.graph.transformations.rules.CreateTypeMember
import puck.jastadd.ExtendJGraphUtils.Rules
import puck.graph.{AccessAbstraction, DependencyGraph, InstanceValueDecl, LoggedError, LoggedSuccess, LoggedTG, NodeId, Parameter, ShowDG, StaticValueDecl, Subtype, SupertypeAbstraction, Type, TypeDecl, UnknownKindType, ValueDef}
import puck.javaGraph.nodeKind.{Field, Method}
import puck.util.{PuckFileLogger, PuckSystemLogger}


object MutantGen {

  def main (args: Array[String]) : Unit = {

    val fn : String =
      if(args.isEmpty) "puck.xml"
      else args.head

    implicit val logger = new PuckSystemLogger(_ => true)

    val p = new Project(ConfigParser(new File(fn)), JavaJastAddDG2AST)
    val dg2ast = p.loadGraph().asInstanceOf[JavaJastAddDG2AST]
    val scm = p.parseConstraints(dg2ast)

    import dg2ast._
    val absMap = initialGraph.abstractionsMap

    import ShowDG._

    val reversedAbsMap = absMap.toList.foldLeft(absMap) {
      case (m, (nid, absSet)) =>
        absSet.toSeq.foldLeft(m) {
          case (m0, AccessAbstraction(absId, SupertypeAbstraction)) =>
            //        println( (initialGraph, nid).shows(desambiguatedFullName) + " abs by " +  (initialGraph, absId).shows(desambiguatedFullName))
            m0 - (nid, AccessAbstraction(absId, SupertypeAbstraction)) +
              (absId, AccessAbstraction(nid, Subtype))
          case (m0, _) => m0
        }
    }

    implicit def idOfFullName(gfn : (DependencyGraph, String)) : NodeId = {
      val (g,fn) = gfn
      DependencyGraph.findElementByName(g, fn).get.id
    }

    def legalMoveCandidate(g : DependencyGraph, id : NodeId) = {

      def notOverriding : Boolean = {

        val hostType = g container_! id
        val superTypes = g abstractions hostType map {
          case AccessAbstraction(id, _) => id
          case _ => puck.error()
        }
        val instanceValueAbs = g abstractions id flatMap (_.nodes)

        val superTypesWithAbstractionOfMoveCandidate =
          superTypes filter (stid => g.content(stid) exists instanceValueAbs.contains)

        superTypesWithAbstractionOfMoveCandidate.isEmpty
      }

      val n = g.getConcreteNode(id)

      def methodWithParameters : Boolean = n.kind match {
        case Method =>
          g.content(id) exists(cid => g.kindType(cid) == Parameter)
        case _ => false
      }


      if (n.mutable && (n.kind.kindType  match {
        case TypeDecl  | StaticValueDecl => true
        case InstanceValueDecl => methodWithParameters && notOverriding

        case _ => false
      })) Some(n)
      else None
    }
    import scala.util.Random

    val mutantLogger = new PuckFileLogger(_=> true, new File("/tmp/mutant.log"))

    import ShowDG._

    def makeRandomMove(num : Int, g : DependencyGraph) : DependencyGraph  =
      if (num == 0 ) g
      else {

        var candidate = legalMoveCandidate(g,  Random.nextInt(g.numNodes))
        while(candidate.isEmpty)
          candidate = legalMoveCandidate(g,  Random.nextInt(g.numNodes))

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
            mutantLogger writeln ("num = " + num)
            mutantLogger writeln ("move candidate = " + (g, n.id).shows(desambiguatedFullName))
            mutantLogger writeln ("container candidate = " + (g, containerId).shows(desambiguatedFullName))

            makeRandomMove(num - 1, g1.mileStone)
          case _ => makeRandomMove(num, g)
        }
      }

    //    val interfaces = initialGraph.nodes.toSeq filter (_.kind == Class)
    //    interfaces.foreach {
    //      n =>
    //        val subtypes = initialGraph.subTypes(n.id)
    //        if(subtypes.nonEmpty) {
    //          (initialGraph, n.id).println(desambiguatedFullName)
    //          println(subtypes map (id =>
    //            "\t" + (initialGraph, id).shows(desambiguatedFullName)) mkString "\n")
    //          println("**************************")
    //        }
    //    }
    //
    //    var g = initialGraph.newGraph(abstractionsMap = reversedAbsMap)
    //
    //    val itc : NodeId = (g, "marauroa.server.db.adapter.DatabaseAdapter")
    //    val impl : NodeId = (g, "marauroa.server.db.adapter.MySQLDatabaseAdapter")
    //    var users = g.usersOf(itc)
    //    users = users.filter(id => (g,id).shows(desambiguatedFullName).startsWith("marauroa"))
    //    while (users.nonEmpty) {
    //
    //      println(users.size)
    //      val LoggedEither(log, \/-(g0)) =
    //        Rules.redirection.
    //          redirectInstanceUsesAndPropagate(g, (users.head, itc), impl)
    //
    //
    //      println(log)
    //      g = g0
    //      users = g.usersOf(itc)
    //      users = users.filter(id => (g,id).shows(desambiguatedFullName).startsWith("marauroa"))
    //    }

    import puck.graph.ConstraintsOps
    val mutant = makeRandomMove(10, initialGraph)
    scm foreach (cm => println((mutant,cm).violations().size + " violations"))
    import puck.util.FileHelper.FileOps
    val recFile = p.workspace \ "mutant.rec"
    Recording.write(recFile.getAbsolutePath, dg2ast.nodesByName, mutant)
    dg2ast(mutant)
    p.outDirectory foreach dg2ast.printCode
  }
}
