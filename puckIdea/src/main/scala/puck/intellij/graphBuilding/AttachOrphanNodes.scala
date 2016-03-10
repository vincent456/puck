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

package puck.intellij.graphBuilding

import com.intellij.psi.{PsiMember, PsiClass}
import puck.graph.{NodeId, DependencyGraph}
import puck.intellij._

/**
 * Created by Loïc Girault on 23/10/15.
 */
object AttachOrphanNodes {
  def apply
  ( fromId : Int = DependencyGraph.rootId )
  ( implicit builder: IntellijGraphBuilder ) : Unit = {
    import builder.g
    val lastId = g.numNodes - 1
    if(fromId < lastId){
      for(nodeId <- Range.inclusive(fromId, lastId) ){
        //println(s"${g.container(nodeId)} contains $nodeId")
        if(g.container(nodeId).isEmpty && nodeId != g.rootId){
          val n = g.getNode(nodeId)
          //println(s"orphan node : ${n.fullName}  : ${n.kind} - container = ${n.container}")
          builder.graph2ASTMap get nodeId foreach {
            case FieldDeclHolder(field) =>
              addOrphanMemberNode(nodeId, field)
              builder.setFieldType(nodeId, field)
            case MethodDeclHolder(method) =>
              addOrphanMemberNode(nodeId, method)
              builder.setMethodType(nodeId, method)
            //            case Some(ConstructorDeclHolder(cdecl)) => addBodyDecl(cdecl)
            case tdh : TypedKindDeclHolder =>
              addOrphanTypeNode(nodeId, tdh.node)
            case PrimitiveDeclHolder(_)
                 | ArrayTypeWrapper => builder.addContains(builder.getPrimitivePackage, nodeId)
            case sdh =>
              println( g.fullName(nodeId) + " " + sdh + " attach orphan nodes unhandled case")
              ()
          }
        }
      }
      //addBodyDecl (case MethodDeclHolder) can add new typeNodes
      apply(lastId)
    }
  }
  def addOrphanTypeNode
  ( classId : NodeId, psiClass: PsiClass)
  ( implicit builder : IntellijGraphBuilder ) : Unit = {
    val pId = psiClass.getContainingClass match {
      case null =>
        builder.addPackage(QualifiedName.packageName(psiClass), mutable = false)
      case parentClass =>
        GetNode(parentClass)
    }
    builder.addContains(pId, classId)
  }
  def addOrphanMemberNode
  ( memberId : NodeId, member : PsiMember)
  ( implicit builder : IntellijGraphBuilder ) : Unit =
    member.getContainingClass match {
      case null => sys.error(s"IntellijGraphBuilder.addOrphanMemberNode(_, ${member.getName}) no containing class")
      case parentClass =>
        val cId = GetNode(parentClass)
        builder.addContains(cId, memberId)
    }

}
