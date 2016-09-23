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

import org.extendj.ast.JastaddGraphBuilder.{qualifierIsSuperAccess, qualifierIsThisAccess}
import puck.PuckError
import puck.graph.{TypeDecl => _, _}
/**
  * Created by Loïc Girault on 05/04/16.
  */

abstract class TypeVariableInstanciator {
  def buildNode( builder : JastaddGraphBuilder) : NodeId
}
case class TVIAccess(a : Access) extends TypeVariableInstanciator{
  def buildNode( builder : JastaddGraphBuilder) : NodeId =
    a.hostBodyDecl() match {
      case bd @ ( _ : StaticInitializer | _ : InstanceInitializer ) =>
        builder.buildNode(bd)
      case bd =>
        builder.definitionOf(builder.buildNode(bd)) match {
          case Some(d) => d
          case None =>
            puck.error(s"no def for ${a.hostBodyDecl().prettyPrint()} in ${a.hostBodyDecl().fullLocation()}")
        }
    }


}
case class TVIVarDecl(d : Variable) extends TypeVariableInstanciator{
  def buildNode( builder : JastaddGraphBuilder) : NodeId = d match {
    case fd : FieldDeclarator => builder.buildNode(fd)
    case vd : VariableDeclarator => builder.buildNode(vd)
    case pd : ParameterDeclaration => builder.buildNode(pd)
  }

}
case class TVITypeDecl(td : TypeDecl) extends TypeVariableInstanciator{
  def buildNode( builder : JastaddGraphBuilder) : NodeId =
    builder.buildNode(td)
}


trait TypeUsage {
  this : JastaddGraphBuilder =>




  def buildTypeUse(tmAccess : TypeMemberAccess,
                   typeMemberUse: NodeIdP,
                   typeMemberUseAccessKind : Option[UsesAccessKind]) : Unit = {
    if(qualifierIsThisAccess(tmAccess)) {
      val thisTypeId = this buildNode tmAccess.hostType()
      val tu = Uses(thisTypeId, thisTypeId)
      addEdge(tu)
      addBinding(tu, typeMemberUse, typeMemberUseAccessKind)
    }
    else if(qualifierIsSuperAccess(tmAccess)){
      val thisTypeId = this buildNode tmAccess.hostType()
      val superTypeId = this buildNode tmAccess.hostType.asInstanceOf[ClassDecl].superclass()
      val tu = Uses(thisTypeId, superTypeId)
      addEdge(tu)
      addBinding(tu, typeMemberUse, typeMemberUseAccessKind)
    }
    else {
      val access = tmAccess.asInstanceOf[Access]
      try getQualifiers(access.qualifier()) foreach {
        qualifier =>
          //if(qualifier.`type`() == access.accessed().hostType())
          if(!qualifier.isSubstitute)
            bindTypeUse(this buildNode qualifier, qualifier,
              typeMemberUse, typeMemberUseAccessKind)
          else {
            val a = qualifier.asInstanceOf[Access]
            a.accessed() match {
              case tms : TypeMemberSubstitute =>
                bindParTypeUse(a, tms, typeMemberUse, typeMemberUseAccessKind)
              case cds : ConstructorDeclSubstituted =>
                bindTypeUse(getNode(cds.getOriginal), cds.getOriginal.hostType(),
                  typeMemberUse, typeMemberUseAccessKind)

              case _ => error()

            }

          }
      }
      catch {
        case e: PuckError =>
          throw e
        //        case e =>
        //          throw new NoTypeUser(s"${access.prettyPrint()}($access) in " +
        //            s"${access.compilationUnit().pathName()} ${access.location()} (${e.getMessage})")
      }
    }
  }

  def bindTypeUse(exprId : NodeId,
                  expr : Expr,
                  typeMemberUse : NodeIdP,
                  typeMemberUseAccessKind : Option[UsesAccessKind]) : Unit =
    bindTypeUse(exprId, expr.`type`(), typeMemberUse, typeMemberUseAccessKind)

  def bindTypeUse(typeUser : NodeId,
                  typeUsed: TypeDecl,
                  typeMemberUse : NodeIdP,
                  typeMemberUseAccessKind : Option[UsesAccessKind]) : Unit = {
    val tid = Type.mainId(getType(typeUsed))
    addBinding(typeUser, tid, typeMemberUse, typeMemberUseAccessKind)
  }


  //out case :
  def normaliseQualifier(qualifier : Expr) : Expr = qualifier match {
    case ae: AssignExpr => normaliseQualifier(ae.getDest)
    case d: Dot =>
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
                     typeMemberUse : NodeIdP,
                     typeMemberUseAccessKind : Option[UsesAccessKind]) : Unit = {
    (qualifierDecl.getOriginalType, qualifier.`type`()) match {
      case ( tv : TypeVariable, _ ) =>
        findTypeVariableInstanciator(tv, qualifierDecl.`type`(), qualifier) foreach {
          case (e, idx) =>
            bindTypeUse(e buildNode this, qualifierDecl.`type`(), typeMemberUse, typeMemberUseAccessKind)
        }

      case (origType : ParTypeDecl,  usageType : ParTypeDecl)
        if origType.genericDecl() == usageType.genericDecl() =>
        //Type variable are ignored here
        bindTypeUse(buildNode(qualifier), qualifier, typeMemberUse, typeMemberUseAccessKind)


      case (_ : ParTypeDecl, _)
           | (_, _ : ParTypeDecl) =>
        println("Access.findTypeUserAndBindUses qualifierDecl.getOriginalType()  " +
          "instanceof ParTypeDecl : TODO !!!")


      case (_, _) if qualifier.`type`().instanceOf(qualifierDecl.getOriginalType) =>
        bindTypeUse(buildNode(qualifier), qualifier, typeMemberUse, typeMemberUseAccessKind)


      case _ => error()
    }
  }

  def findTypeVariableInstanciator
  (tv : TypeVariable,
   tvValue : TypeDecl,
   expr : Expr) : Option[(TypeVariableInstanciator, Int)]= {

    (expr, tv.owner()) match {
      case (ca : ClassInstanceExpr, _ ) => ??? //Some(TVIAccess(ca))
      case (d : AbstractDot, _ ) => findTypeVariableInstanciator(tv, tvValue, d.lastAccess())
      case (a : Access, gmd : GenericMethodDecl ) =>
        if(a.accessed() == gmd) Some((TVIAccess(a), gmd indexOfTypeVariable tv))
        else {
          println("findTypeVariableInstanciator tv.owner() : GenericMethodDecl " +
            "a.accessed() != tv.owner() : TODO !!!")
          None
        }
      case (a : Access, gtd : GenericTypeDecl) =>

        val tvOwnerTd = gtd.asInstanceOf[TypeDecl]

        import scala.collection.JavaConversions._
        def findTVInstanciatiorInHierachy(subtype : TypeDecl)  =
          tvOwnerTd.childTypes() find subtype.instanceOf match {
            case Some(typUser) =>
              addEdge(Uses(buildNode(typUser), buildNode(tvValue)))
              Some((TVITypeDecl(typUser), gtd.indexOfTypeVariable(tv)))

            case None if subtype instanceOf tvOwnerTd =>
              addEdge(Uses(buildNode(subtype), buildNode(tvValue)))
              Some((TVITypeDecl(subtype), gtd.indexOfTypeVariable(tv)))

            case None =>
              println("findTVInstanciatiorInHierachy failure :" +
                s"${expr.prettyPrint()} : ${expr.getClass} in " +
                s"${expr.compilationUnit().pathName()} line ${expr.location()}")
              None
          }

        (a.accessed(), a.`type`()) match {
          case (d : Variable, ptd : ParTypeDecl) if ptd.genericDecl() == gtd =>
            Some((TVIVarDecl(a.accessed().asInstanceOf[Variable]), gtd indexOfTypeVariable tv))

          case (d : Declarator, td) if td instanceOf tvOwnerTd =>
            findTVInstanciatiorInHierachy(td)

          case _ if a.isQualified =>
            findTypeVariableInstanciator(tv, tvValue, a.prevExpr())

          case _ =>
            findTVInstanciatiorInHierachy(a.hostType())
        }

      case (_, owner) =>
        println(s"findTypeVariableInstanciator(tv = ${tv.name}, tvValue = ${tvValue.name()}, " +
          s"expr = ${expr.prettyPrint()}), tvOwner.getClass = ${owner.getClass} : TODO !!!")
        None
    }
  }



  def constraintTypeUses
  (lvalueTCVar : TypeConstraintVariable,
   lvalueType : TypeDecl,
   rvalue : Access) : Unit =
    if(rvalue.`type`().isInstanceOf[AnonymousDecl])
      println("Access.constraintTypeUses with Anonymous Class not handled yet !")
    else if(rvalue.isInstanceOf[ClassInstanceExpr]) {
      rvalue.`type`() match {
        case ptd : ParTypeDecl => ()
        case _ =>
          addTypeConstraint(Sub(TypeOf(buildNode(rvalue.lastAccess())), lvalueTCVar) )
      }
    }
    else lvalueType match {
      case lvalueParType : ParTypeDecl =>
        if(lvalueParType.isRawType) println(s"Raw ParType at ${rvalue.fullLocation()} : no type constraint generated")
        else constraintParTypeUses(lvalueTCVar, lvalueParType, rvalue.lastAccess())
      case _  if rvalue.isSubstitute =>

        rvalue.accessed() match {
          case tms : TypeMemberSubstitute if tms.getOriginalType.isInstanceOf[TypeVariable] =>
            val tv = tms.getOriginalType.asInstanceOf[TypeVariable]
            findTypeVariableInstanciator(tv, tms.`type`(), rvalue) foreach {
              case (e, idx) =>
                addTypeConstraint(Sub(ParTypeProjection(TypeOf(e buildNode this), idx),
                  lvalueTCVar))
            }
          case _ =>
            addTypeConstraint(Sub(TypeOf(buildNode(rvalue.lastAccess())), lvalueTCVar) )
        }

      case _ =>
        addTypeConstraint(Sub(TypeOf(buildNode(rvalue.lastAccess())), lvalueTCVar))
    }

  def constraintParTypeUses
  ( lvalueTCVar : TypeConstraintVariable,
    lvalueType : ParTypeDecl,
    rvalue : Access) : Unit =
    if(!rvalue.`type`().isInstanceOf[ParTypeDecl])
      println("Access.constraintTypeUses this.type() " +
        "not instanceof ParTypeDecl  : TODO !!!")
    else  rvalue.`type`() match {
      case rvalueType : ParTypeDecl if rvalueType.isRawType =>
        println("raw rvalue type, type constraint not generated")
      case rvalueType : ParTypeDecl =>

        if(rvalueType.numTypeParameter() != lvalueType.numTypeParameter())
          println("Access.constraintTypeUses " +
            "rvalueType.getNumArgument() != lValueType.getNumArgument()  : TODO !!!")
        else {
          //Hypothesis : argument are in same order  C<B,A> extends I<A,B> kind of case not handled
          addTypeConstraint(Sub( TypeOf(this buildNode rvalue) , lvalueTCVar))

          def normalizeTypeDecl(t : TypeDecl) : TypeDecl = t match {
            case _ : WildcardType => rvalue.program().typeObject()
            case wst : WildcardSuperType if wst.getAccess.isInstanceOf[TypeAccess] =>
              wst.getAccess.asInstanceOf[TypeAccess].decl()
            case wet : WildcardExtendsType if wet.getAccess.isInstanceOf[TypeAccess] =>
              wet.getAccess.asInstanceOf[TypeAccess].decl()
            case _ => t
          }

          /*Range(0, lvalueType.numTypeParameter()).foreach {
            i =>
              val lvalueArg = normalizeTypeDecl(lvalueType.getParameterization.getArg(i))
              val rvalueArg = normalizeTypeDecl(rvalueType.getParameterization.getArg(i))
              if(lvalueArg.isObject)
                System.err.println(rvalue.fullLocation() + " lvalue object as type argument not constrained")
              else {
                val tvValueId =  buildNode(lvalueArg)

                val tv: TypeVariable = rvalueArg match {
                  case tv: TypeVariable => tv
                  case _ => rvalueType.genericDecl().asInstanceOf[GenericTypeDecl].getTypeParameter(i)
                }

                findTypeVariableInstanciator(tv, lvalueArg, rvalue) foreach (
                  e =>
                    addTypeConstraint(Eq((e buildNode this, tvValueId), TypeOf(lvalue))))
              }
          }*/
        }
    }


  def normalizeAccessAndApply
  (f : Access => Unit,
   default : Expr => Unit) : PartialFunction[Expr, Unit] = {
    case a : Access => f(a)
    case p : ParExpr => normalizeAccessAndApply(f, default)(p.getExpr)
    case ce : ConditionalExpr =>
      normalizeAccessAndApply(f,default)(ce.getFalseExpr)
      normalizeAccessAndApply(f,default)(ce.getTrueExpr)
    case a : AssignSimpleExpr => a.getSource
      normalizeAccessAndApply(f,default)(a.getSource)
      normalizeAccessAndApply(f,default)(a.getDest)
    case ioe : InstanceOfExpr =>
      System.err.println("type constraint, InstanceOfExpr not handled")
    case ace : ArrayCreationExpr =>
      System.err.println("type constraint, ArrayCreationExpr not handled")
    case (_ : Binary | _ : Unary)=>
      System.err.println("type constraint, Unary or Binary expr not handled")
    case _: Literal | _ : CastExpr  => ()
    case e => default(e)
  }

  private def putConstraintOnArg_common[T](f : PartialFunction[(T,Access), Unit])
                                          (p : (T, Expr) ) : Unit = {
    val (t, argValue) = p

    normalizeAccessAndApply(
      a => f((t, a)),
      arg => throw new DGBuildingError(s"Access expected but found : $arg " + arg.fullLocation())
    )(argValue)

  }

  protected val putConstraintOnArg: ((ParameterDeclaration, Expr)) => Unit =
    putConstraintOnArg_common[ParameterDeclaration] {
      case (param, arg: Access) =>
        val paramId = this buildNode param
        constraintTypeUses(TypeOf(paramId), param.`type`(), arg)
    }

  protected def putConstraintOnSubstitutedArg(ma : MethodAccess): (((ParameterDeclaration,ParameterDeclaration), Expr)) => Unit =
    putConstraintOnArg_common[(ParameterDeclaration,ParameterDeclaration)] {
      case ((genParam, substituteParam), arg: Access) =>
        val paramId = this buildNode genParam

        (genParam.`type`(), substituteParam.`type`()) match {
          case (tv : TypeVariable, tvValue) =>
            findTypeVariableInstanciator(tv, tvValue, ma)  foreach {
              case (e, idx) =>
                constraintTypeUses(ParTypeProjection(TypeOf(e buildNode this), idx), tvValue, arg)
            }


          case (genT, subT) if genT == subT =>
            constraintTypeUses(TypeOf(paramId), genT, arg)
          case (genT, subT) =>
            System.err.println(s"(genT = ${genT.name()}, subT = ${subT.name()}) arg type constraint unhandled case")
        }

    }

}
