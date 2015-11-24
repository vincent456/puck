package puck.jastadd
package concretize

import puck.graph._
import puck.graph.transformations.{Target, RedirectionOp, Regular, Transformation}
import puck.javaGraph._
import puck.util.PuckLogger
import ShowDG._
import org.extendj.{ast => AST}

object RedirectTarget {

  def setType
  ( graph: DependencyGraph,
    reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    typeUser : NodeId, typeId: NodeId)
  (implicit logger : PuckLogger) : Unit = {
    logger.writeln(s"setting type of ${(reenactor, typeUser).shows} " +
      s"to ${(reenactor, typeId).shows}")
    (id2declMap(typeUser), id2declMap(typeId)) match {
      case (MethodDeclHolder(mdecl), tk: TypedKindDeclHolder) =>
        mdecl.setTypeAccess(tk.decl.createLockedAccess())
      case (ParameterDeclHolder(fdecl), tk: TypedKindDeclHolder) =>
        fdecl.setTypeAccess(tk.decl.createLockedAccess())
      case (FieldDeclHolder(fdecl), tk: TypedKindDeclHolder) =>
        fdecl.setTypeAccess(tk.decl.createLockedAccess())
    }
  }
    def apply
  ( graph: DependencyGraph,
    reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    e: DGEdge, newTargetId: NodeId)
  ( implicit logger : PuckLogger) : Unit =  {
    logger.writeln(s"redirecting ${(reenactor, e).shows} " +
      s"target to ${(reenactor, newTargetId).shows}")
    if(e.target != newTargetId) {
      val target = reenactor.getNode(e.target)
      val source = reenactor.getNode(e.source)
      val newTarget = reenactor.getNode(newTargetId)


      val sourceInAST = id2declMap(source.id)

      (id2declMap(target.id), id2declMap(newTarget.id)) match {
        case (InterfaceDeclHolder(odlDecl), InterfaceDeclHolder(newDecl))
          if e.kind == Isa =>
          sourceInAST match {
            case ClassDeclHolder(srcDecl) =>
              srcDecl.replaceImplements(odlDecl.createLockedAccess(), newDecl.createLockedAccess())
            case _ => throw new JavaAGError("isa arc should only be between TypeKinds")
          }

        case (oldk: TypedKindDeclHolder, newk: TypedKindDeclHolder) =>
           sourceInAST match {
             case BlockHolder(block) =>
               val MethodDeclHolder(mdecl) = id2declMap(reenactor.container_!(e.source))

               //TODO find why !(block eq mdecl.getBlock)
               //logger.writeln(block eq mdecl.getBlock)

               mdecl.getBlock.replaceTypeAccess(oldk.decl.createLockedAccess(), newk.decl.createLockedAccess())

             case holder : HasNode =>
               holder.node.replaceTypeAccess(oldk.decl.createLockedAccess(), newk.decl.createLockedAccess())

             case k => throw new JavaAGError(k + " as user of TypeKind, redirection unhandled !")
          }



        case (oldk: ClassDeclHolder, newk: FieldDeclHolder) =>
          // assume this a case replace this.m by delegate.m
          logger.writeln()
          logger.writeln()
          logger.writeln("*** REPLACE THIS QUALIFIER")
          val t = Transformation(Regular, RedirectionOp(e, Target(newTargetId)))
          logger.writeln((reenactor, t).shows)
          //TODO refine
          (sourceInAST, reenactor styp newTarget.id) match {
            case (bdh : HasBodyDecl, Some(NamedType(fieldType))) =>
              logger.write("*** typeUse ")
              logger.writeln((reenactor, Uses(newTargetId, fieldType)).shows)
              logger.writeln("type member uses " + reenactor.typeMemberUsesOf(newTargetId, fieldType))
              logger.writeln()
              logger.writeln()

              reenactor.typeMemberUsesOf(newTargetId, fieldType).foreach{
                methodUse =>
                  id2declMap(methodUse.used) match {
                    case ConcreteMethodDeclHolder(mdecl)=>
                      val fieldAccess = newk.decl.createLockedAccess()
                      bdh.decl.replaceThisQualifierFor(mdecl, fieldAccess)
                    case _ => throw  new JavaAGError("unhandled case !")
                  }
              }

            case _ =>
              val t = Transformation(Regular, RedirectionOp(e, Target(newTargetId)))
              throw new JavaAGError((reenactor, t).shows + " unhandled")
          }


        case (FieldDeclHolder(oldDecl), FieldDeclHolder(newDecl)) =>
          sourceInAST match {
            case defHolder : DefHolder =>
              defHolder.node.replaceFieldAccess(oldDecl, newDecl.createLockedAccess())
            case k =>
              throw new JavaAGError(k + " as user of Field, redirection unhandled !")
          }


        case (oldk: MethodDeclHolder, newk: MethodDeclHolder) =>
          sourceInAST match {
            case defHolder : DefHolder =>
              val MethodDeclHolder(mdecl) = id2declMap(reenactor.container_!(e.source))
              mdecl.replaceMethodCall(oldk.decl, newk.decl)

            //TODO find why !(block eq mdecl.getBlock)
            //logger.writeln(block eq mdecl.getBlock)


            case k =>
              throw new JavaAGError(k + " as user of Method, redirection unhandled !")
          }

        case (ConstructorDeclHolder(oldc), ConstructorDeclHolder(newc)) =>
          sourceInAST match {
            case BlockHolder(block) =>
              val MethodDeclHolder(mdecl) = id2declMap(reenactor.container_!(e.source))

              //TODO find why !(block eq mdecl.getBlock)
              logger.writeln(block eq mdecl.getBlock)

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
          println("source = " + source)
          println("target = " + target)
          println("new target = " + newTarget)
          throw new JavaAGError("redirecting TARGET of %s to %s : application failure !".format(e, newTarget))
      }
    }
  }
}
