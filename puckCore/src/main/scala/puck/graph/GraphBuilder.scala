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

trait GraphBuilder {
  var g : DependencyGraph = _

  var nodesByName = Map[String, NodeId]()

  var fromLibrary = Set[NodeId]()

  def getNodeByName( k : String) : NodeId = nodesByName(k) //java accessor

  def getFullName( id : NodeId) : String = g.fullName(id)

  def addNode(unambiguousFullName: String,
              localName: String,
              kind: NodeKind)
             (onCreate : NodeId => Unit = _ => () ) : NodeId = {
    nodesByName get unambiguousFullName match {
      case None =>
        val (n, g2) = g.addConcreteNode(localName, kind)
        //println(s"adding ${n.id} $unambiguousFullName ")
        this.nodesByName += (unambiguousFullName -> n.id)
        g = g2
        onCreate(n.id)
        n.id
      case Some(id) => id
    }
  }

  def setType(id : NodeId, typ : Type): Unit ={
    g = g.addType(id, typ)
  }

  def addContains(containerId : NodeId, contentId : NodeId): Unit ={
    g = g.addContains(containerId, contentId)
  }

  def addEdge(e : DGEdge): Unit =
    g = g.addEdge(e)


  def addIsa(subType: Type, superType: Type): Unit =
    g = g.addIsa(subType, superType)


  def addTypeConstraint(constraint : TypeConstraint): Unit =
    g = g.addTypeConstraint(constraint)



  def addBinding( typeUse : NodeIdP,
                  typeMemberUse : NodeIdP,
                  typeMemberUseAccessKind : Option[UsesAccessKind]): Unit = {
    g = g.addBinding(typeUse, typeMemberUse)
    typeMemberUseAccessKind foreach {
      accK =>
        g = g.addAccessKind((typeUse, typeMemberUse), accK)
    }
  }


  def addBinding(typeUser : NodeId, typeUsed:NodeId,
                 typeMemberUse : NodeIdP,
                 typeMemberUseAccessKind : Option[UsesAccessKind]) : Unit =
    addBinding((typeUser, typeUsed), typeMemberUse, typeMemberUseAccessKind)


  def addParams(decl : NodeId, params : List[Int]) : Unit = {
    params.reverseIterator.foreach{ //order matters
      param =>
        g = g.addEdge(ContainsParam(decl, param))
    }
  }

  def registerSuperTypeAbtractions() = {
    val superTypesMap = g.edges.superTypes.content
    superTypesMap foreach {
      case (subId, directSuperTypes) =>
        val directSuperTypesIds = directSuperTypes map Type.mainId
        val subMethIds = g instanceValuesWithType subId
        g = directSuperTypesIds.foldLeft(g) {
          (g, supId) =>
            g.addAbstraction(subId, AccessAbstraction(supId, SupertypeAbstraction))
        }
        g = registerMethodsDirectOverridingInTypeHierarchy(g, subMethIds, directSuperTypesIds)
    }
  }

  def registerMethodsDirectOverridingInTypeHierarchy
  (g : DependencyGraph,
   subMethods : List[(ConcreteNode, Type)],
   directSuperTypes : Set[NodeId]) : DependencyGraph = {
    directSuperTypes.foldLeft(g) {
      case (g0, superType) =>
        val (g1, remainings) = registerMethodsDirectOverridingInSuperType(g, subMethods, superType)
        registerMethodsDirectOverridingInTypeHierarchy(g1, remainings, g directSuperTypesId superType)
    }
  }

  def registerMethodsDirectOverridingInSuperType
  (g : DependencyGraph,
   subMethods : List[(ConcreteNode, Type)],
   superType : NodeId) : (DependencyGraph, List[(ConcreteNode, Type)]) = {
    val overriddenCandidates = g instanceValuesWithType superType

    val (g1, nonOverridingsMethods, _) = subMethods.foldLeft((g, List[(ConcreteNode, Type)](), overriddenCandidates)){
      case ((g0, remainingSubs, remainingCandidates), typedMeth @ (subMeth, subMethSig)) =>
        Type.findOverridingIn(g0, subMeth.name, subMethSig, remainingCandidates) match {
          case None => (g0, typedMeth :: remainingSubs, remainingCandidates)
          case Some(((absM,_), remainingCandidates1)) =>
            (g0.addAbstraction(subMeth.id, AccessAbstraction(absM.id, SupertypeAbstraction)),
              remainingSubs, remainingCandidates1)
        }
    }
    (g1, nonOverridingsMethods)
  }


  //  def registerAbstraction(graph: DependencyGraph, implId : NodeId, abs: Abstraction) : DependencyGraph =
  //    abs match {
  //      case AccessAbstraction(absId, SupertypeAbstraction) =>
  //        val impl = graph.getConcreteNode(implId)
  //        val absNode = graph.getConcreteNode(absId)
  //        val absMeths = g instanceValuesWithType absId
  //        val candidates = g instanceValuesWithType impl.id
  //        Type.findAndRegisterOverridedInList(graph, absMeths, candidates) {
  //          Type.ignoreOnImplemNotFound
  //          //errorOnImplemNotFound(graph.fullName(impl.id))
  //        } match {
  //          case LoggedSuccess(_, g) => g.addAbstraction(implId, abs)
  //          case LoggedError(_, err) => throw err
  //        }
  //
  //      case _ => g.addAbstraction(implId, abs)
  //    }
}
