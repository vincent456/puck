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

  def buildTypeVariables(tid : NodeId, gtd : GenericTypeDecl) : Unit =
    gtd.getTypeParameterList foreach (tv =>
      addEdge(ContainsParam(tid, this buildNode tv)))



  def buildTypeDecl(td : TypeDecl) : NodeId = td match {
    case tv : TypeVariable => this buildNode tv
    case cd : ClassDecl =>

      val n = this buildNode cd

      // addExtends
      if(cd.hasSuperclass && cd.superclass().fullName() != "java.lang.Object"){
        addIsa(n, this buildNode cd.getSuperClass)
        cd.getSuperClass.buildIsaEdges(this, n)
      }

      TypeDecl.addImplements(this, n, cd.getImplementss)

      if(cd.isGenericType)
        buildTypeVariables(n, cd.asInstanceOf[GenericTypeDecl])

      if(cd.hasImplicitConstructor)
        cd.getImplicitConstructor.buildDG(this, n)

      n
    case id : InterfaceDecl =>
      val n = this buildNode id
      TypeDecl.addImplements(this, n, id.getSuperInterfaces)
      if(id.isGenericType)
        buildTypeVariables(n, id.asInstanceOf[GenericTypeDecl])

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


    val ctorNodeId = this buildNode expr //does lock
    if( expr.hasTypeDecl )
      expr setTarget null//unlock anonymous decl

    addEdge(Uses(containerId, ctorNodeId))
  }

  def buildDG(containerId : NodeId, stmt : EnhancedForStmt) : Unit = {
    stmt.buildDGInChildren(this, containerId)
    stmt.getExpr match {
      case access : Access =>
        val superTypeUser = this buildNode stmt.getVariableDecl
        val superTypeUsed = this buildNode stmt.getTypeAccess

        val typUsed = stmt.getExpr.`type`().asInstanceOf[ParTypeDecl]
        val subTypeUsed = this buildNode typUsed.getParameterization.getArg(0)

        getQualifiers(access.lastAccess()) foreach {
          typBinder =>
          val subTypeUser = this buildNode typBinder

          addTypeUsesConstraint((superTypeUser, superTypeUsed),
            Sub((subTypeUser, subTypeUsed)))
        }
      case _ => println("type constraint in : " + stmt.prettyPrint() + " not handled")
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
          case a: Access => constraintTypeUses(containerId, astType, a)
          case _ => ()
        }
    }
  }

  def buildDG(containerId : NodeId, expr : AssignExpr) : Unit = {
    expr.getSource.buildDG(this, containerId)
    expr.getDest.buildDG(this, containerId)

    val astType = expr.getDest.`type`()
    val destNode = expr.getDest match {
      case a : Access => buildNode(a.lastAccess())
      case _ => throw new DGBuildingError()
    }

    expr.getSource match {
      case src : Access =>
        constraintTypeUses(destNode, astType, src.lastAccess())
      case _ => ()
    }
  }

  def buildDG(containerId : NodeId, member : FieldDecl) : Unit = {
    val t = getType(member.getTypeAccess)

    val astType = member.`type`()

    member.getDeclarators.foreach {
      fd =>
        val fdId = this buildNode fd
        addContains(containerId, fdId)
        setType(fdId, t)
        // t.ids.foreach (id => addEdge(Uses(fdId, id))) is not needed see EdgeMap.uses

        if( fd.hasInit ) {
          puck.ignore(buildFieldInit(fdId, fd, fd.getInit))
          fd.getInit match {
            case a: Access => constraintTypeUses(fdId, astType, a)
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

  def buildDG(containerId : NodeId, pd : ParameterDeclaration) : Unit = {
    buildNode(pd)
    pd.buildDGInChildren(this, containerId)
  }

  def buildDG(containerId : NodeId, va : ConstructorAccess) : Unit = {
    addEdge(Uses(containerId, buildNode(va)))
    va.buildDGInChildren(this, containerId)
  }
  def buildDG(containerId : NodeId, va : VarAccess) : Unit =
    if(va. decl().isField){
      val nodeId = this buildNode va
      val typeMemberUses = Uses(containerId, nodeId, va.usesAccessKind())
      addEdge(typeMemberUses)

      if(!va.isDeclStatic)
        buildTypeUse(va, typeMemberUses)
    }

  def buildDG(containerId : NodeId, ta : TypeAccess) : Unit =
    addEdge(Uses(containerId, this buildNode ta))




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

    //99% du temps il y'a une déclaration mais dans des cas ou une classe implémente
    //plusieurs interfaces indépendante possédant une signature commune, on peux avoir plusieurs déclaration
    decls foreach {
      decl =>
        val nodeId = this buildNode decl.asInstanceOf[DGNamedElement]
        val typeMemberUses = Uses(containerId, nodeId)
        addEdge(typeMemberUses)

        if(!decl.isStatic)
          buildTypeUse(ma, typeMemberUses)

        if(! decl.isSubstitute)
          decl.getParameterList.toList.zip(ma.getArgs.toList) foreach putConstraintOnArg
         else {
          val substitutedDecl = decl.asInstanceOf[MethodDeclSubstituted]
          val genDecl = substitutedDecl.sourceMethodDecl()

          genDecl.getParameterList.toList.zip(
            substitutedDecl.getParameterList.toList).zip(
            ma.getArgs.toList) foreach putConstraintOnSubstitutedArg(ma)

        }
    }
  }

  private def putConstraintOnArg_common[T](f : PartialFunction[(T,Access), Unit])
                                     (p : (T, Expr) ) : Unit = {
    val (t, argValue) = p
    argValue match {
      case a: Access => f((t, a))
      case ae: ArithmeticExpr =>
        System.err.println("parameter type constraint, ArithmeticExpr case not handled")
      case _: Literal => ()
      case arg => throw new DGBuildingError(s"Access expected but found : $arg")
    }
  }

  private val putConstraintOnArg: ((ParameterDeclaration, Expr)) => Unit =
    putConstraintOnArg_common[ParameterDeclaration] {
      case (param, arg: Access) =>
        val paramId = this buildNode param
        constraintTypeUses(paramId, param.`type`(), arg)
    } _
  private def putConstraintOnSubstitutedArg(ma : MethodAccess): (((ParameterDeclaration,ParameterDeclaration), Expr)) => Unit =
    putConstraintOnArg_common[(ParameterDeclaration,ParameterDeclaration)] {
      case ((genParam, substituteParam), arg: Access) =>
        val paramId = this buildNode genParam

        (genParam.`type`(), substituteParam.`type`()) match {
          case (tv : TypeVariable, tvValue) =>
            findTypeVariableInstanciator(tv, tvValue, ma)  foreach (
              e => constraintTypeUses(e buildNode this, tvValue, arg) )

          case (genT, subT) if genT == subT =>
            constraintTypeUses(paramId, genT, arg)
          case (genT, subT) =>
            System.err.println(s"(genT = ${genT.name()}, subT = ${subT.name()}) arg type constraint unhandled case")
        }

    } _


  def buildDG(containerId : NodeId, rs : ReturnStmt) : Unit = {
    rs.buildDGInChildren(this, containerId)
    rs.getResult match {
      case a : Access =>
        val methodNode = this buildNode rs.hostBodyDecl()
        val astType = rs.hostBodyDecl().asInstanceOf[MethodDecl].`type`()
        constraintTypeUses(methodNode, astType, a.lastAccess())

      case _ : Literal | _ : CastExpr=> ()
      case resExpr => throw new DGBuildingError("buildDG ReturnStmt case not expected : " + resExpr)
    }

  }

}
