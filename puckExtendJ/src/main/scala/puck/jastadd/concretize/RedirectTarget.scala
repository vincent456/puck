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

package puck.jastadd
package concretize

import org.extendj.ast.{TypedKindDeclHolder, _}
import puck.ignore
import puck.graph.{TypeDecl => PTypeDecl, _}
import puck.graph.transformations.{RedirectionOp, Regular, Target, Transformation}
import puck.javaGraph._
import puck.util.PuckLogger
import ShowDG._

import scala.collection.JavaConversions._

object RedirectTarget {

  def changeType
  ( typeUser : NodeId,
    oldTypeId : NodeId,
    typeId: NodeId)
  ( implicit logger : PuckLogger,
     resultAndReenactor : (DependencyGraph, DependencyGraph),
     id2declMap: NodeId => ASTNodeLink ) : Unit = {

    val (_, reenactor) = resultAndReenactor

    val log = s"change type in ${(reenactor, typeUser).shows} " +
      s"from ${(reenactor, oldTypeId).shows}" +
      s" to ${(reenactor, typeId).shows}"

    logger writeln log

    val TypedKindDeclHolder(tdecl) = id2declMap(typeId)
    val TypedKindDeclHolder(oldTdecl) = id2declMap(oldTypeId)

    id2declMap(typeUser) match {
      case BlockHolder(block) =>

        val MethodDeclHolder(mdecl) = id2declMap(reenactor container_! typeUser)
         //TODO find why !(block eq mdecl.getBlock)
        logger.writeln("block eq mdecl.getBlock = " + (block eq mdecl.getBlock))
        ignore(mdecl.getBlock.replaceTypeAccess(oldTdecl, tdecl))

      case FieldDeclHolder(decl,_) =>
        ignore(decl.replaceTypeAccess(oldTdecl, tdecl))
      case LocalVarDeclHolder(declarator) =>
        var n: ASTNode[_] = declarator.getParent
        while(! (n.isInstanceOf[VarDeclStmt] ||
          n.isInstanceOf[EnhancedForStmt])){
          n = n.getParent
        }
        ignore(n.replaceTypeAccess(oldTdecl, tdecl))
      case holder : HasNode =>
        ignore(holder.node.replaceTypeAccess(oldTdecl, tdecl))

      case k => throw new JavaAGError(k + " as user of TypeKind, redirection unhandled !")
    }
  }

  def redirectUse
  ( e: DGEdge,
    target : DGNode,
    newTarget: DGNode)
  ( implicit logger : PuckLogger,
    resultAndReenactor : (DependencyGraph, DependencyGraph),
    id2declMap: NodeId => ASTNodeLink ) : Unit = {
    val (resultGraph, reenactor) = resultAndReenactor
    val source = reenactor.getNode(e.source)
    val sourceInAST = id2declMap(source.id)

    (id2declMap(target.id), id2declMap(newTarget.id)) match {
      case (oldk: ClassDeclHolder, newk: FieldDeclHolder) =>
        // assume this a case replace this.m by delegate.m
        logger.writeln()
        logger.writeln()
        logger.writeln("*** REPLACE THIS QUALIFIER")
        val t = Transformation(Regular, RedirectionOp(e, Target(newTarget.id)))
        logger.writeln((reenactor, t).shows)
        //TODO refine
        (sourceInAST, reenactor styp newTarget.id) match {
          case (bdh : HasBodyDecl, Some(NamedType(fieldType))) =>
            logger.write("*** typeUse ")
            logger.writeln((reenactor, Uses(newTarget.id, fieldType)).shows)
            logger.writeln("type member uses " + reenactor.typeMemberUsesOf(newTarget.id, fieldType))
            logger.writeln()
            logger.writeln()

            reenactor.typeMemberUsesOf(newTarget.id, fieldType).foreach{
              methodUse =>
                id2declMap(methodUse.used) match {
                  case MethodDeclHolder(mdecl)=>
                    val fieldAccess = newk.decl.getDeclarator(newk.declaratorIndex).createLockedAccess()
                    bdh.decl.replaceThisQualifierFor(mdecl, fieldAccess)
                  case _ => throw  new JavaAGError("unhandled case !")
                }
            }

          case _ =>
            val t = Transformation(Regular, RedirectionOp(e, Target(newTarget.id)))
            throw new JavaAGError((reenactor, t).shows + " unhandled")
        }


      case (oldF : FieldDeclHolder, newF : FieldDeclHolder) =>
        sourceInAST match {
          case defHolder : DefHolder =>
            defHolder.node.replaceFieldAccess(oldF.declarator, newF.declarator.createLockedAccess())
          case k =>
            throw new JavaAGError(k + " as user of Field, redirection unhandled !")
        }

      case (oldF : FieldDeclHolder, MethodDeclHolder(mdecl)) =>
        sourceInAST match {
          case defHolder : DefHolder =>
            if(mdecl.getNumParameter == 0) //assume it's a getter
              defHolder.node.replaceFieldReadAccess(oldF.declarator, mdecl)
            else
              defHolder.node.replaceFieldWriteAccess(oldF.declarator, mdecl)
//            reenactor.usesAccessKind(e) match {
//              case Some(Read) =>
//                defHolder.node.replaceFieldReadAccess(oldF.declarator, mdecl)
//              case Some(Write) =>
//                defHolder.node.replaceFieldWriteAccess(oldF.declarator, mdecl)
//              case Some(RW) =>
//                defHolder.node.replaceFieldReadAccess(oldF.declarator, mdecl)
//                defHolder.node.replaceFieldWriteAccess(oldF.declarator, mdecl)
//              case None =>
//                throw new JavaAGError("redirecting uses of field toward getter/setter access kind unknown")
//            }


          case k =>
            throw new JavaAGError(k + " as user of Field, redirection unhandled !")
        }

      case (MethodDeclHolder(oldDecl), MethodDeclHolder(newDecl)) =>
        sourceInAST match {
          case defHolder : DefHolder =>
            //TODO find why !(block eq mdecl.getBlock)
            //logger.writeln(block eq mdecl.getBlock)

            val CallableDeclHolder(cdecl) = id2declMap(reenactor.container_!(e.source))
            cdecl.replaceMethodCall(oldDecl, newDecl)

          case ClassDeclHolder(decl) =>
            decl.getBodyDeclList foreach {
              case bd @ ( _ : StaticInitializer | _ : InstanceInitializer) =>
                bd.replaceMethodCall(oldDecl, newDecl)
              case _ => ()
            }

          case k : HasNode =>
            throw new JavaAGError(s"${k.node.dgFullName()} as user of Method" +
              s" ${oldDecl.dgFullName()}, redirection unhandled !")
          case k =>
            throw new JavaAGError(k + " as user of Method, redirection unhandled !")
        }

      case (ConstructorDeclHolder(oldc), ConstructorDeclHolder(newc)) =>
        sourceInAST match {
          case BlockHolder(block) =>
            val MethodDeclHolder(mdecl) = id2declMap(reenactor.container_!(e.source))

            //TODO find why !(block eq mdecl.getBlock)
            mdecl.getBlock.replaceConstructorCall(oldc, newc)

          case defHolder : DefHolder =>
            defHolder.node.replaceConstructorCall(oldc, newc)

          case src =>
            throw new JavaAGError(s"constructor change, ${src.getClass} as uses source unhandled")
        }

      case (ConstructorDeclHolder(cdecl), methCtor: MethodDeclHolder) =>
        sourceInAST match {
          case ConstructorDeclHolder(_) =>
            throw new JavaAGError("redirection to constructor method within " +
              "constructor no implemented (see methodDecl)")
          case dh: DefHolder => dh.node.replaceByConstructorMethodCall(cdecl, methCtor.decl)

          case k =>
            throw new JavaAGError(k + " as user of MethodKind, redirection unhandled !")
        }


      case _ =>
        throw new JavaAGError("redirecting TARGET of %s to %s : application failure !".format(e, newTarget))
    }
  }


  def apply
  ( e: DGEdge, newTargetId: NodeId)
  ( implicit logger : PuckLogger,
    resultAndReenactor : (DependencyGraph, DependencyGraph),
    id2declMap: NodeId => ASTNodeLink ) : Unit = {
    val (resultGraph, reenactor) = resultAndReenactor

    logger.writeln(s"redirecting ${(reenactor, e).shows} " +
      s"target to ${(reenactor, newTargetId).shows}")
    if(e.target != newTargetId) {
        val target = reenactor.getNode(e.target)
        val newTarget = reenactor.getNode(newTargetId)

        if (target.kind.kindType == PTypeDecl &&
          newTarget.kind.kindType == PTypeDecl)
          changeType(e.source, e.target, newTargetId)
        else
          redirectUse(e, target, newTarget)

      }

  }
}
