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

  def buildDG(containerId : NodeId, td : TypeDecl) : Unit = td match {
    case _ : WildcardExtendsType | _ : WildcardSuperType => ()
    case _ =>
      val n = buildTypeDecl(td)
      addContains(containerId, n)
      if(td.isParameterizedType){
        val ptd = td.asInstanceOf[ParTypeDecl]
        println("[BUILD] parameterized type " + ptd.nameWithArgs())
      }
      td.getBodyDeclList foreach (_.buildDG(this, n))
  }

  def buildTypeDecl(td : TypeDecl) : NodeId = td match {
    case tv : TypeVariable =>
      tv buildDGNode this
    case cd : ClassDecl =>

      val n = cd buildDGNode this

      // addExtends
      if(cd.hasSuperclass && cd.superclass().fullName() != "java.lang.Object"){
        addIsa(n, cd.getSuperClass buildDGNode this)
        cd.getSuperClass.buildIsaEdges(this, n)
      }


      TypeDecl.addImplements(this, n, cd.getImplementss)

      if(cd.isGenericType)
        cd.asInstanceOf[GenericClassDecl].buildDG_TypeParameters(this, n)

      if(cd.hasImplicitConstructor)
        cd.getImplicitConstructor.buildDG(this, n)

      n
    case id : InterfaceDecl =>
      val n = id buildDGNode this
      TypeDecl.addImplements(this, n, id.getSuperInterfaces)
      if(id.isGenericType)
        id.asInstanceOf[GenericInterfaceDecl].buildDG_TypeParameters(this, n)
      n

    case _ =>
      throw new DGBuildingError(s"ClassDecl or InterfaceDecl exptected, $td found");
  }

  def buildDG(containerId : NodeId, pta : ParTypeAccess) : Unit = {
    getType(pta).ids.foreach(id => addEdge(Uses(containerId, id)))
    pta.buildDGInChildren(this, containerId)
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

  def buildDG(containerId : NodeId, stmt : EnhancedForStmt) : Unit = {
    stmt.buildDGInChildren(this, containerId)
    stmt.getExpr match {
      case access : Access =>
        val typeUser = stmt.getVariableDecl buildDGNode this
        val typeUsed = stmt.getTypeAccess buildDGNode this
        findTypeUserAndBindUses(Uses(typeUser, typeUsed), access.lastAccess())
      case _ => ()
    }
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

  def buildDG(containerId : NodeId, expr : AssignExpr) : Unit = {
    expr.getSource.buildDG(this, containerId)
    expr.getDest.buildDG(this, containerId)

    val astType = expr.getDest.`type`()
    val destNode = expr.getDest match {
      case a : Access => a.lastAccess().accessed().buildDGNode(this)
      case _ => throw new DGBuildingError()
    }

    expr.getSource match {
      case src : Access =>
        constraintTypeUses(destNode, astType, src.lastAccess().buildDGNode(this), src.lastAccess())
      case _ => ()
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
    theDef.buildDGInChildren(this, defId)
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

  def buildDG(containerId : NodeId, ta : TypeAccess) : Unit =
    addEdge(Uses(containerId, ta buildDGNode this))

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
    ma.getArgs.foreach{
      expr => expr.buildDG(this, containerId)
    }
    decls.map{
      case mds : MethodDeclSubstituted => mds.sourceMethodDecl()
      case md => md
    }.foreach{
      decl =>
        val nodeId = decl buildDGNode this
        val typeMemberUses = Uses(containerId, nodeId)
        addEdge(typeMemberUses)

        if(!decl.isStatic)
          buildTypeUse(ma, typeMemberUses)

        decl.getParameterList.toList.zip(ma.getArgs.toList).foreach{
          case (param, arg : Access) if param.`type`().isInstanceOf[TypeVariable] =>
            System.err.println("parameter type constraint, param typed with a type variable case not handled")
          case (param, arg : Access) =>
            val paramId = param buildDGNode this
            val argId = arg buildDGNode this
            constraintTypeUses(paramId, param.`type`(), argId, arg)
          case (_, ae : AddExpr) =>
            System.err.println("parameter type constraint, AddExpr case not handled")
          case (_, _ : Literal) =>()
          case (_, arg) =>
            throw new DGBuildingError(s"Access expected but found : $arg")
        }
    }

  }


  def buildDG(containerId : NodeId, rs : ReturnStmt) : Unit = {
    rs.buildDGInChildren(this, containerId)
    rs.getResult match {
      case a : Access =>
        val methodNode = rs.hostBodyDecl().buildDGNode(this)
        val astType = rs.hostBodyDecl().asInstanceOf[MethodDecl].`type`()
        constraintTypeUses(methodNode, astType, a.lastAccess().buildDGNode(this), a.lastAccess())

      case _ : Literal | _ : CastExpr=> ()
      case resExpr => throw new DGBuildingError("buildDG ReturnStmt case not expected : " + resExpr)
    }

  }

}
