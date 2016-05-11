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

import puck.graph.{TypeDecl => PuckTypeDecl, _}

/**
  * Created by Loïc Girault on 05/04/16.
  */
trait TypeUsage {
  this : JastaddGraphBuilder =>


  def bindTypeUse(exprId : NodeId,
                  expr : Expr,
                  typeMemberUse : Uses) : Unit =
    bindTypeUse(exprId, expr.`type`(), typeMemberUse)

  def bindTypeUse(typeUser : NodeId,
                  typeUsed: TypeDecl,
                  typeMemberUse : Uses/*,
                  constraint : NodeIdP => TypeUseConstraint = Sub.apply*/ ) : Unit ={
    g.kindType(typeMemberUse.used) match {
      case PuckTypeDecl => /*error("bindTypeUse error !!!!")*/
        addTypeUsesConstraint(typeMemberUse, Sub((typeUser, typeMemberUse.used)))
      case _ =>
        val tid = getType(typeUsed) match {
          case NamedType(id) => id
          case ParameterizedType(id, _) => id
        }
        puck.ignore(addBinding(typeUser, tid, typeMemberUse))
    }
  }

  def findTypeUserAndBindUses
  ( typeMemberUse : Uses,
    qualifier : Expr) : Unit = {

    def aux(qualifier : Access) : Unit = qualifier.accessed() match {
      case accessed : TypeMemberSubstitute =>
        findTypeUserAndBindUses(typeMemberUse, qualifier, accessed)
      case _ =>
        bindTypeUse(qualifier buildDGNode this, qualifier, typeMemberUse)
    }

    qualifier match {
      case ae: AssignExpr => findTypeUserAndBindUses(typeMemberUse, ae.getDest)
      case d: Dot =>
        if (d.isRightRotated)
          d.rotateLeft()
        findTypeUserAndBindUses(typeMemberUse, d.getRight)
      case pe: ParExpr =>
        findTypeUserAndBindUses(typeMemberUse, pe.getExpr)
      case aa: ArrayAccess =>
        findTypeUserAndBindUses(typeMemberUse, aa.prevExpr())
      case ce: ConditionalExpr =>
        findTypeUserAndBindUses(typeMemberUse, ce.getFalseExpr)
        findTypeUserAndBindUses(typeMemberUse, ce.getTrueExpr)

      // findTypeUserAndBindUses term cases
      case ae: AddExpr =>
        bindTypeUse(ae.hostBodyDecl buildDGNode this, ae, typeMemberUse)
      case ce: CastExpr =>
        bindTypeUse(ce.getTypeAccess buildDGNode this, ce, typeMemberUse)

      case l: Literal =>
        bindTypeUse(l.`type`() buildDGNode this, l, typeMemberUse)
      // findTypeUserAndBindUses term cases - Access subclasses

      case cie: ClassInstanceExpr if cie.hasTypeDecl =>
        aux(cie)
        cie.setTarget(null) //unlock anonymous typeDecl
      case va : VarAccess =>
        aux(va)
        // do not lock varAccess
        va.setTarget(null)
      case a : Access => aux(a)

      case _ =>
        throw new DGBuildingError(qualifier.prettyPrint() + "(" + qualifier.getClass + ") in " +
          qualifier.compilationUnit().pathName() + " line " +
          qualifier.location() + " typeUser not found")
    }
  }

  def findTypeUserAndBindUses(typeMemberUse : Uses,
                              qualifier : Access,
                              qualifierDecl : TypeMemberSubstitute) : Unit = {
    (qualifierDecl.getOriginalType, qualifier.`type`()) match {
      case ( tv : TypeVariable, _ ) =>
        tv.owner() match {
          case tvOwner : GenericTypeDecl =>
            findTypeVariableInstanciatorAndBindUses(tv, tvOwner,
                  qualifierDecl.`type`(), typeMemberUse,
                  qualifier buildDGNode this, qualifier)
          case _ =>
            println("Access.findTypeUserAndBindUses tv.owner() " +
              "not instanceof GenericTypeDecl : TODO !!!")
        }

      case (_ : ParTypeDecl, _)
           | (_, _ : ParTypeDecl) =>
        println("Access.findTypeUserAndBindUses qualifierDecl.getOriginalType()  " +
          "instanceof ParTypeDecl : TODO !!!")

      case (_, _) if qualifier.`type`().instanceOf(qualifierDecl.getOriginalType) =>
        bindTypeUse(qualifier.buildDGNode(this), qualifier, typeMemberUse)

      case _ => error()
    }
  }





  def addBinding(user : TypeDecl, used : TypeDecl, tmUse : Uses) : Unit = {
    val tUser = user.buildDGNode(this)
    val tUsed = used.buildDGNode(this)
    addEdge(addBinding(tUser, tUsed, tmUse))
  }

  def findTypeVariableInstanciatorAndBindUses
  (tv : TypeVariable,
   tvOwner : GenericTypeDecl,
   tvValue : TypeDecl,
   typeMemberUse : Uses,
   typeUserNodeId : NodeId,
   e: Expr) : Unit = {
    e match {
      case a : Access =>
        a.`type`() match {
          case ptd : ParTypeDecl =>
            val ge = ptd.genericDecl().asInstanceOf[GenericElement]
            if(ge.ownTypeVariable(tv))
              g.kindType(typeMemberUse.used) match {
                case PuckTypeDecl =>
                  addTypeUsesConstraint(typeMemberUse, Eq((typeUserNodeId, typeMemberUse.used)))
                case _ => bindTypeUse(a buildDGNode this, tvValue, typeMemberUse)
              }

              //addTypeUsesConstraint(typeMemberUse, Sub((a.buildDGNode(this), typeMemberUse.used)))
              //bindTypeUse(a.buildDGNode(this), tvValue, typeMemberUse)

            else
              println("Access.findTypeVariableInstanciatorAndBindUses " +
                "not ge.ownTypeVariable(tv) : TODO !!!")
          case t =>
            val tvOwnerTd = tvOwner.asInstanceOf[TypeDecl]

            import scala.collection.JavaConversions._

            def findTypeUserInHierachyAnBindUse(subtype : TypeDecl) : Unit =
              tvOwnerTd.childTypes() find subtype.instanceOf match {
                case Some(typUser) => addBinding(typUser, tvValue, typeMemberUse)

                case None if subtype instanceOf tvOwnerTd =>
                  addBinding(subtype, tvValue, typeMemberUse)
                case None =>
                  throw new NoTypeUser(s"${e.prettyPrint()} : ${e.getClass} in " +
                  s"${e.compilationUnit().pathName()} line ${e.location()}")
              }

            if(t.instanceOf(tvOwnerTd))
              findTypeUserInHierachyAnBindUse(t)
            else if(a.isQualified)
                findTypeVariableInstanciatorAndBindUses(tv, tvOwner, tvValue,
                  typeMemberUse, a.prevExpr() buildDGNode this, a.prevExpr())
            else
                findTypeUserInHierachyAnBindUse(a.hostType())
        }

      case o => println(s"${o.getClass}.findTypeVariableInstanciatorAndBindUses : TODO !!!")
    }
  }



  def constraintTypeUses
  ( lvalue : NodeId,
    lvalueType : TypeDecl,
    rvalueNodeId : NodeId,
    rvalue : Access): Unit =
    if(rvalue.`type`().isInstanceOf[AnonymousDecl])
      println("Access.constraintTypeUses with Anonymous Class not handled yet !")
    else lvalueType match {
      case lvalueParType : ParTypeDecl =>
        constraintParTypeUses(lvalue, lvalueParType, rvalueNodeId, rvalue.lastAccess())
      case _ =>
        if(rvalue.lastAccess().accessed().isInstanceOf[Substitute] ||
          rvalue.lastAccess().`type`().isInstanceOf[Substitute])
          findTypeUserAndBindUses(Uses(lvalue, lvalueType.buildDGNode(this)),
            rvalue.lastAccess().getQualifier)
        else
          addTypeUsesConstraint((lvalue, lvalueType buildDGNode this),
            Sub((rvalue.lastAccess() buildDGNode this,
            rvalue.lastAccess().`type`() buildDGNode this)))
    }



  def constraintParTypeUses
  ( lvalue : NodeId,
    lvalueType : ParTypeDecl,
    rvalueNodeId : NodeId,
    rvalue : Access) : Unit =
    if(!rvalue.`type`().isInstanceOf[ParTypeDecl])
      println("Access.constraintTypeUses this.type() " +
        "not instanceof ParTypeDecl  : TODO !!!")
    else {
      val rValueType = rvalue.`type`().asInstanceOf[ParTypeDecl]
      if(rValueType.numTypeParameter() != lvalueType.numTypeParameter())
        println("Access.constraintTypeUses " +
          "rValueType.getNumArgument() != lValueType.getNumArgument()  : TODO !!!")
      else {
        //Hypothesis : argument are in same order  C<B,A> extends I<A,B> kind of case not handled
        val rvalueNode = rvalue buildDGNode this
        addTypeUsesConstraint((lvalue, lvalueType.genericDecl() buildDGNode this),
          Sub( (rvalueNode, rValueType.genericDecl() buildDGNode this) ))

        def normalizeTypeDecl(t : TypeDecl) : TypeDecl = t match {
          case _ : WildcardType => rvalue.program().typeObject()
          case wst : WildcardSuperType if wst.getAccess.isInstanceOf[TypeAccess] =>
            wst.getAccess.asInstanceOf[TypeAccess].decl()
          case wet : WildcardExtendsType if wet.getAccess.isInstanceOf[TypeAccess] =>
            wet.getAccess.asInstanceOf[TypeAccess].decl()
          case _ => t
        }

        Range(0, lvalueType.numTypeParameter()).foreach {
          i =>
            val lvalueArg = normalizeTypeDecl(lvalueType.getParameterization.getArg(i))
            val rvalueArg = normalizeTypeDecl(rValueType.getParameterization.getArg(i))

            val tv : TypeVariable = rvalueArg match {
              case tv : TypeVariable => tv
              case _ => rValueType.genericDecl().asInstanceOf[GenericTypeDecl].getTypeParameter(i)
            }

            findTypeVariableInstanciatorAndBindUses(tv, tv.owner().asInstanceOf[GenericTypeDecl],
              lvalueArg, Uses(lvalue, lvalueArg buildDGNode this), rvalueNodeId, rvalue)
        }
      }
    }


}
