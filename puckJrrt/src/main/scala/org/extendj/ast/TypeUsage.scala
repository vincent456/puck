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


  def bindTypeUse(typeUser : NodeId, typeUsed: TypeDecl, typeMemberUse : Uses) : Unit ={
    g.kindType(typeMemberUse.used) match {
      case PuckTypeDecl =>
        // getType(typeUsed).ids contains typeMemberUse.used
        addTypeUsesConstraint(typeMemberUse,(typeUser, typeMemberUse.used))
      case _ =>
        val tid = getType(typeUsed) match {
          case NamedType(id) => id
          case ParameterizedType(id, _) => id
        }
        addBinding(typeUser, tid, typeMemberUse)
    }

  }

  def findTypeUserAndBindUses(typeMemberUse : Uses,
                              qualifier : Access,
                              qualifierDecl : TypeMemberSubstitute) : Unit = {
    (qualifierDecl.getOriginalType, qualifier.`type`()) match {
      case ( tv : TypeVariable, _ ) =>
        tv.owner() match {
          case tvOwner : GenericTypeDecl =>
            val qualifierHostType = qualifier.asInstanceOf[TypeMemberAccess].decl().hostType()
            findTypeVariableInstanciatorAndBindUses(tv, tvOwner,
                  qualifierDecl.`type`(), typeMemberUse, qualifier)
          case _ =>
            println("Access.findTypeUserAndBindUses tv.owner() " +
              "not instanceof GenericTypeDecl : TODO !!!")
        }

      case (_ : ParTypeDecl, _)
           | (_, _ : ParTypeDecl) =>
        println("Access.findTypeUserAndBindUses qualifierDecl.getOriginalType()  " +
          "instanceof ParTypeDecl : TODO !!!")

      case (_, _) if qualifier.`type`().instanceOf(qualifierDecl.getOriginalType) =>
        qualifier.bindTypeUse(this, qualifier.buildDGNode(this), typeMemberUse)

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
   e: Expr){
    e match {
      case a : Access =>
        a.`type`() match {
          case ptd : ParTypeDecl =>
            val ge = ptd.genericDecl().asInstanceOf[GenericElement]
            if(ge.ownTypeVariable(tv))
              bindTypeUse(a.buildDGNode(this), tvValue, typeMemberUse)
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
                findTypeVariableInstanciatorAndBindUses(tv, tvOwner, tvValue, typeMemberUse, a.prevExpr())
            else
                findTypeUserInHierachyAnBindUse(a.hostType())
        }

      case o => println(s"${o.getClass}.findTypeVariableInstanciatorAndBindUses : TODO !!!")
    }
  }



  def constraintTypeUses
  ( lvalue : NodeId,
    lvalueType : TypeDecl,
    rvalue : Access): Unit =
    if(rvalue.`type`().isInstanceOf[AnonymousDecl])
      println("Access.constraintTypeUses with Anonymous Class not handled yet !")
    else lvalueType match {
      case lvalueParType : ParTypeDecl =>
        constraintParTypeUses(lvalue, lvalueParType, rvalue.lastAccess())
      case _ =>
        if(rvalue.lastAccess().accessed().isInstanceOf[Substitute] ||
          rvalue.lastAccess().`type`().isInstanceOf[Substitute])
          rvalue.lastAccess().findTypeUserAndBindUses(this,
            Uses(lvalue, lvalueType.buildDGNode(this)))
        else
          addTypeUsesConstraint(lvalue,
            lvalueType buildDGNode this,
            rvalue.lastAccess() buildDGNode this,
            rvalue.lastAccess().`type`() buildDGNode this )
    }



  def constraintParTypeUses
  ( lvalue : NodeId,
    lvalueType : ParTypeDecl,
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
        addTypeUsesConstraint(lvalue,
          lvalueType.genericDecl() buildDGNode this,
          rvalueNode,
          rValueType.genericDecl() buildDGNode this)

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

            (lvalueArg, rvalueArg) match {
              case (_, tv : TypeVariable) =>
                findTypeVariableInstanciatorAndBindUses(tv, tv.owner().asInstanceOf[GenericTypeDecl],
                  lvalueArg, Uses(lvalue, lvalueArg buildDGNode this), rvalue)
              case _ =>
                addTypeUsesConstraint(lvalue, lvalueArg buildDGNode this,
                  rvalueNode, rvalueArg buildDGNode this)
            }
        }
      }
    }


}
