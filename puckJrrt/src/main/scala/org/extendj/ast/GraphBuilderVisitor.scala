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

package org.extendj.ast

import puck.graph.{Uses, _}
import scala.collection.JavaConversions._
/**
  * Created by Loïc Girault on 18/04/16.
  */
trait GraphBuilderVisitor {
  this : JastaddGraphBuilder =>

  def buildDG(pta : ParTypeAccess, containerId : NodeId) : Unit = {
    getType(pta).ids.foreach(id => addEdge(Uses(containerId, id)))
  }

  def buildDG(containerId : NodeId, expr : ClassInstanceExpr) : Unit = {
    expr.getAccess match {
      case _ :TypeAccess => ()
      case pta : ParTypeAccess =>
        pta.getTypeArguments.foreach {
          ta => ta.buildDG(this, containerId)
        }
    }

    expr.getArgList.buildDG(this, containerId)
    expr.getTypeDeclOpt.buildDG(this, containerId)


    val ctorNodeId = expr buildDGNode this //does lock
    if( expr.hasTypeDecl )
      expr setTarget null//unlock anonymous decl

    addEdge(Uses(containerId, ctorNodeId))
  }

  def buildDG(containerId : NodeId, stmt : VarDeclStmt) : Unit = {
    val t = getType(stmt.getTypeAccess)
    t.ids.foreach (id => addEdge(Uses(containerId, id)))

    val astType = stmt.`type`()

    stmt.getDeclarators filter(_.hasInit) foreach {
      vd =>
        vd.getInit.buildDG(this, containerId)
        vd.getInit match {
          case a: Access => constraintTypeUses(containerId, astType, containerId, a)
          case _ => ()
        }
    }
  }

  def buildDG(containerId : NodeId, member : FieldDecl) : Unit = {
    val t = getType(member.getTypeAccess)

    val astType = member.`type`()

    member.getDeclarators.foreach {
      fd =>
        val fdId = fd buildDGNode this
        addContains(containerId, fdId)
        setType(fdId, t)
        // t.ids.foreach (id => addEdge(Uses(fdId, id))) is not needed see EdgeMap.uses

        if( fd.hasInit ) {
          val defId = buildFieldInit(fdId, fd, fd.getInit)
          fd.getInit match {
            case a: Access => constraintTypeUses(fdId, astType, defId, a)
            case _ => ()
          }
        }
    }
  }

  def buildFieldInit(fieldId : NodeId, field : FieldDeclarator, init : Expr) : NodeId ={
    val defId = getDefNode(field)
    registerDef(defId, init)
    addContains(fieldId, defId)
    init.buildDG(this, defId)
    defId
  }

  def buildDef(defOwner : DGNamedElement,
               theDef : Block , defOwnerId: NodeId) : NodeId = {
    val defId = getDefNode(defOwner)
    theDef.registerDef(this, defId)
    addContains(defOwnerId, defId)
    theDef.buildDG(this, defId)
    defId
  }

  def buildDG(containerId : NodeId, va : VarAccess) : Unit =
    if(va. decl().isField){
      val  nodeId = va buildDGNode this
      val typeMemberUses = Uses(containerId, nodeId, va.usesAccessKind())
      addEdge(typeMemberUses)

      if(!va.isDeclStatic)
        buildTypeUse(va, typeMemberUses)
    }

  def buildDG(containerId : NodeId, ma : MethodAccess) : Unit = {
    if(!ma.isSubstitute)
      ma.lock()
    val decls = ma.decls_keepMethodsInDifferentTypeHierarchy()
    if(!decls.isSingleton()){
      println(s"Warning ! method access ${ma.name()}" +
        s" in ${ma.compilationUnit().pathName()} line ${ma.location()}" +
        " refers to several declaration : ")
      decls.foreach(d => println(d.dgFullName()))
    }
    decls.foreach{
      decl =>
        val nodeId = decl buildDGNode this
        val typeMemberUses = Uses(containerId, nodeId)
        addEdge(typeMemberUses)

        if(!decl.isStatic)
          buildTypeUse(ma, typeMemberUses)
    }
    ma.getArgs.foreach{
      expr => expr.buildDG(this, containerId)
    }
  }


}
