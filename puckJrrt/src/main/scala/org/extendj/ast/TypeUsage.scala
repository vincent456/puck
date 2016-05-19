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

import JastaddGraphBuilder.{qualifierIsSuperAccess, qualifierIsThisAccess}
/**
  * Created by Loïc Girault on 05/04/16.
  */

abstract class TypeVariableInstanciator {
  def buildNode( builder : JastaddGraphBuilder) : NodeId
}
case class TVIAccess(a : Access) extends TypeVariableInstanciator{
  def buildNode( builder : JastaddGraphBuilder) : NodeId =
    builder.getDefinition(builder.buildNode(a.hostBodyDecl()))

}
case class TVIVarDecl(d : Declarator) extends TypeVariableInstanciator{
  def buildNode( builder : JastaddGraphBuilder) : NodeId = d match {
    case fd : FieldDeclarator => builder.buildNode(fd)
    case vd : VariableDeclarator => builder.buildNode(vd)
  }

}
case class TVITypeDecl(td : TypeDecl) extends TypeVariableInstanciator{
  def buildNode( builder : JastaddGraphBuilder) : NodeId =
    builder.buildNode(td)
}


trait TypeUsage {
  this : JastaddGraphBuilder =>


  def buildTypeUse(tmAccess : TypeMemberAccess, typeMemberUse: Uses) : Unit = {
    if(qualifierIsThisAccess(tmAccess)) {
      val thisTypeId = this buildNode tmAccess.hostType()
      addEdge(addBinding(thisTypeId, thisTypeId, typeMemberUse))
    }
    else if(qualifierIsSuperAccess(tmAccess)){
      val thisTypeId = this buildNode tmAccess.hostType()
      val superTypeId = this buildNode tmAccess.hostType.asInstanceOf[ClassDecl].superclass()
      addEdge(addBinding(thisTypeId, superTypeId, typeMemberUse))
    }
    else {
      val access = tmAccess.asInstanceOf[Access]
      try getQualifiers(access.qualifier()) foreach {
        qualifier =>
          //if(qualifier.`type`() == access.accessed().hostType())
          if(!qualifier.isSubstitute)
            bindTypeUse(this buildNode qualifier, qualifier, typeMemberUse)
          else {
              val a = qualifier.asInstanceOf[Access]
              bindParTypeUse(a, a.accessed().asInstanceOf[TypeMemberSubstitute], typeMemberUse)
          }
      }
      catch {
        case _: Error =>
          throw new NoTypeUser(access.prettyPrint() + "(" + access.getClass + ") in " +
            access.compilationUnit().pathName() + " " + access.location())
      }
    }
  }

  def bindTypeUse(exprId : NodeId,
                  expr : Expr,
                  typeMemberUse : NodeIdP) : Unit =
    bindTypeUse(exprId, expr.`type`(), typeMemberUse)

  def bindTypeUse(typeUser : NodeId,
                  typeUsed: TypeDecl,
                  typeMemberUse : NodeIdP) : Unit = {
        val tid = getType(typeUsed) match {
          case NamedType(id) => id
          case ParameterizedType(id, _) => id
          case _ => error()
        }
        puck.ignore(addBinding(typeUser, tid, typeMemberUse))
  }


  //out case :
  def normaliseQualifier(qualifier : Expr) : Expr = qualifier match {
    case ae: AssignExpr => normaliseQualifier(ae.getDest)
    case d: Dot =>
      if (d.isRightRotated)
        d.rotateLeft()
      normaliseQualifier(d.getRight)
    case pe: ParExpr =>
      normaliseQualifier(pe.getExpr)
    case aa: ArrayAccess =>
      normaliseQualifier(aa.prevExpr())
    case _ => qualifier

  }

  def getQualifiers(qualifier : Expr) : Seq[Expr] = {

    normaliseQualifier(qualifier) match {
      case ce: ConditionalExpr =>
        getQualifiers(ce.getFalseExpr) ++
        getQualifiers(ce.getTrueExpr)

      // findTypeUserAndBindUses term cases
      case e @ ( _ : AddExpr | _ : CastExpr | _ : Literal) =>
        Seq(e)

      case a : Access => Seq(a)

      case _ =>
        throw new DGBuildingError(qualifier.prettyPrint() + "(" + qualifier.getClass + ") in " +
          qualifier.compilationUnit().pathName() + " line " +
          qualifier.location() + " typeUser not found")
    }
  }


  def bindParTypeUse(qualifier : Access,
                     qualifierDecl : TypeMemberSubstitute,
                     typeMemberUse : NodeIdP) : Unit = {
    (qualifierDecl.getOriginalType, qualifier.`type`()) match {
      case ( tv : TypeVariable, _ ) =>
        tv.owner() match {
          case tvOwner : GenericTypeDecl =>
            findTypeVariableInstanciator(tv, tvOwner, qualifierDecl.`type`(), qualifier) foreach (
               e =>
                 bindTypeUse(e buildNode this, qualifierDecl.`type`(), typeMemberUse)
              )


          case _ =>
            println("Access.findTypeUserAndBindUses tv.owner() " +
              "not instanceof GenericTypeDecl : TODO !!!")

        }

      case (_ : ParTypeDecl, _)
           | (_, _ : ParTypeDecl) =>
        println("Access.findTypeUserAndBindUses qualifierDecl.getOriginalType()  " +
          "instanceof ParTypeDecl : TODO !!!")


      case (_, _) if qualifier.`type`().instanceOf(qualifierDecl.getOriginalType) =>
        bindTypeUse(buildNode(qualifier), qualifier, typeMemberUse)


      case _ => error()
    }
  }


  def addBinding(user : TypeDecl, used : TypeDecl, tmUse : NodeIdP) : Unit = {
    val tUser = this buildNode user
    val tUsed = this buildNode used
    addEdge(addBinding(tUser, tUsed, tmUse))
  }



  def findTypeVariableInstanciator
  (tv : TypeVariable,
   tvOwner : GenericElement,
   tvValue : TypeDecl,
   e: Expr) : Option[TypeVariableInstanciator]= {
    e match {
      case ca : ClassInstanceExpr => Some(TVIAccess(ca))

      case a : Access =>
        a.`type`() match {
          case ptd : ParTypeDecl =>
            val ge = ptd.genericDecl().asInstanceOf[GenericElement]
            if(ge.ownTypeVariable(tv)) Some(TVIVarDecl(a.accessed().asInstanceOf[Declarator]))

            else {
              println("Access.findTypeVariableInstanciatorAndBindUses " +
                "not ge.ownTypeVariable(tv) : TODO !!!")
              None
            }
          case t =>
            val tvOwnerTd = tvOwner.asInstanceOf[TypeDecl]

            import scala.collection.JavaConversions._

            def findTypeUserInHierachyAnBindUse(subtype : TypeDecl)  =
              tvOwnerTd.childTypes() find subtype.instanceOf match {
                case Some(typUser) =>
                  addEdge(Uses(buildNode(typUser), buildNode(tvValue)))
                  Some(TVITypeDecl(typUser))
                  //addBinding(typUser, tvValue, typeMemberUse)

                case None if subtype instanceOf tvOwnerTd =>
                  addEdge(Uses(buildNode(subtype), buildNode(tvValue)))
                  Some(TVITypeDecl(subtype))
                  //addBinding(subtype, tvValue, typeMemberUse)
                case None =>
                  println(s"${e.prettyPrint()} : ${e.getClass} in " +
                  s"${e.compilationUnit().pathName()} line ${e.location()}")
                  None
              }

            if(t.instanceOf(tvOwnerTd))
              findTypeUserInHierachyAnBindUse(t)
            else if(a.isQualified)
                findTypeVariableInstanciator(tv, tvOwner, tvValue, a.prevExpr())
            else
                findTypeUserInHierachyAnBindUse(a.hostType())
        }

      case o =>
        println(s"${o.getClass}.findTypeVariableInstanciatorAndBindUses : TODO !!!")
        None
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
      case _  if rvalue.isSubstitute =>
        val tms = rvalue.accessed().asInstanceOf[TypeMemberSubstitute]

        tms.getOriginalType match {
          case tv : TypeVariable =>
            findTypeVariableInstanciator(tv, tv.owner(), tms.`type`(), rvalue) foreach {
              e =>
                addTypeUsesConstraint((lvalue, buildNode(lvalueType)),
                  Sub((e buildNode this, buildNode(tms.`type`())) ))
            }
          case _ =>
            addTypeUsesConstraint((lvalue, this buildNode lvalueType),
              Sub((buildNode(rvalue.lastAccess()), buildNode(rvalue.lastAccess().`type`())) ))
        }
      case _ =>
          addTypeUsesConstraint((lvalue, this buildNode lvalueType),
            Sub((buildNode(rvalue.lastAccess()), buildNode(rvalue.lastAccess().`type`())) ))
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
        val rvalueNode = this buildNode rvalue
        addTypeUsesConstraint((lvalue, this buildNode lvalueType.genericDecl()),
          Sub( (rvalueNode, this buildNode rValueType.genericDecl()) ))

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

            val tvValueId = {
              val lid = buildNode(lvalueArg)
              val rid = buildNode(rvalueArg)
              if(lid != rid) error()
              lid
            }

            val tv : TypeVariable = rvalueArg match {
              case tv : TypeVariable => tv
              case _ => rValueType.genericDecl().asInstanceOf[GenericTypeDecl].getTypeParameter(i)
            }

            // findTypeVariableInstanciatorAndBindUses(tv, tv.owner().asInstanceOf[GenericTypeDecl],
            //lvalueArg, Uses(lvalue, lvalueArg buildDGNode this), rvalueNodeId, rvalue)


            findTypeVariableInstanciator(tv, tv.owner().asInstanceOf[GenericTypeDecl],
              lvalueArg, rvalue) foreach {
              e =>
                addTypeUsesConstraint((lvalue, tvValueId), Eq((e buildNode this, tvValueId)))
            }
        }
      }
    }


}
