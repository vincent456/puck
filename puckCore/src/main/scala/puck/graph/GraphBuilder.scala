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

package puck.graph

import puck.graph.constraints.SupertypeAbstraction

trait GraphBuilder {
  var g : DependencyGraph = _
  type NodeIdT = NodeId
  var nodesByName = Map[String, NodeIdT]()

  def getNodeByName( k : String) : NodeIdT = nodesByName(k) //java accessor

  def getFullName( id : NodeIdT) : String = g.fullName(id)

  def addNode(unambiguousFullName: String,
              localName: String,
              kind: NodeKind,
              mutable : Boolean)
             (onCreate : NodeId => Unit = _ => () ): NodeIdT = {
    nodesByName get unambiguousFullName match {
      case None =>
        val (n, g2) = g.addConcreteNode(localName, kind, mutable)
        //println(s"adding ${n.id} $unambiguousFullName ")
        this.nodesByName += (unambiguousFullName -> n.id)
        g = g2
        onCreate(n.id)
        n.id
      case Some(id) => id
    }
  }

  def setMutability(id : NodeIdT, mutable : Boolean): Unit ={
    g = g.setMutability(id, mutable)
  }

  def setType(id : NodeIdT, typ : Type): Unit ={
    g = g.addType(id, typ)
  }

  def addContains(containerId : NodeIdT, contentId : NodeIdT): Unit ={
    g = g.addContains(containerId, contentId)
  }

  def addEdge(e : DGEdge): Unit =
    g = g.addEdge(e)


  def addIsa(subTypeId: NodeIdT, superTypeId: NodeIdT): Unit =
    g = g.addIsa(subTypeId, superTypeId)


  def addTypeUsesConstraint(superTypeUse : NodeIdP,
                            constraint : TypeUseConstraint): Unit =
    g = g.addTypeUsesConstraint(superTypeUse, constraint)



  def addBinding( typeUse : NodeIdP,
                  typeMemberUse : NodeIdP): Unit =
     g = g.addBinding(typeUse, typeMemberUse)


  def addBinding(typeUser : NodeId, typeUsed:NodeId, typeMemberUse : NodeIdP) : Uses ={
//    import ShowDG._
//    (g,typeUse).println
//    (g,typeMemberUse).println

    addBinding((typeUser, typeUsed), typeMemberUse)
    Uses(typeUser, typeUsed)
  }

  def addParams(decl : NodeId, params : List[Int]) : Unit = {
    params.reverseIterator.foreach{ //order matters
      param =>
        g = g.addEdge(ContainsParam(decl, param))
    }
  }

  type ImplId = NodeId
  type AbsId = NodeId

  def registerAbstraction : DependencyGraph => (ImplId, Abstraction) => DependencyGraph =
    graph => (implId , abs) => graph.addAbstraction(implId, abs)


  def registerSuperTypes() =
    g = g.isaEdges.foldLeft(g){ (g, e) =>
      registerAbstraction(g)(e.source, AccessAbstraction(e.target, SupertypeAbstraction))
    }

}
