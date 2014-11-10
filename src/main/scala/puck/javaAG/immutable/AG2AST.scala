package puck.javaAG.immutable

import AST.CompilationUnit
import puck.graph.AccessGraph
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.immutable.AccessGraph.NodeId
import puck.graph.immutable.{Uses, Isa, Contains, AGEdge}
import puck.graph.immutable.transformations._
import puck.javaAG.JavaAGError
import puck.javaAG.immutable.nodeKind._
import puck.util.{PuckLog, PuckLogger, PuckNoopLogger}

/**
 * Created by lorilan on 23/07/14.
 */
class AG2AST(val program : AST.Program) {
  var logger: PuckLogger = PuckNoopLogger
  implicit val defaultVerbosity = (PuckLog.AG2AST, PuckLog.Info)

  def apply(reenactor: JavaAccessGraph,
            resultGraph : AccessGraph,
            t: Transformation) : AccessGraph = t match {
    case Transformation(Add, TTNode(id, name, kind, styp, mutable, dh: DeclHolder)) =>
      //redo t before createDecl
      val n = resultGraph.getNode(id)
      n.t.asInstanceOf[DeclHolder].createDecl(program, resultGraph, id)

      //redo t after applying on code other transformations
    case `t` => t match {
      case Transformation(Add, TTEdge(e)) =>
        //println("creating edge " + e)
        add(resultGraph, e)

      case Transformation(Add, TTRedirection(e, Target(newTarget))) =>
        logger.writeln("redirecting %s target to %s".format(e, newTarget))
        redirectTarget(resultGraph, e, newTarget)

      case Transformation(Add, TTRedirection(e, Source(newSource))) =>
        redirectSource(resultGraph, reenactor, e, newSource); t.redo(reenactor)
      /*case Transformation(Remove(), TTEdge(e@AGEdge(Contains(), source, target))) =>
    println("removing edge " + e)
    (source.kind, target.kind) match {
    case (p@Package(), i: TypeKind) =>
      i.decl.compilationUnit().setPackageDecl("unkown")

    case (i@Interface(), m@AbstractMethod()) =>
      i.decl.removeBodyDecl(m.decl)

    case (c@Class(), m@Method()) =>
      c.decl.removeBodyDecl(m.decl)

    case _ => println("%s not removed".format(e))

    }*/

      case Transformation(_, TTAbstraction(impl, abs, SupertypeAbstraction)) =>
        (reenactor.getNode(impl).t, reenactor.getNode(abs).kind) match {
          case (ConcreteMethodDeclHolder(Some(decl)), AbstractMethod) =>
            decl.setVisibility(AST.ASTNode.VIS_PUBLIC)
          case _ => ()
        }

      case Transformation(_, TTAbstraction(_, _, _)) => ()

      case Transformation(Remove, TTNode(id, name, kind, styp, mutable, dh: TypedKindDeclHolder)) =>
        if (dh.decl.nonEmpty)
          dh.decl.get.deleteAGNode()

      case _ => logger.writeln("%s not applied on program".format(t))
    }
    resultGraph
  }


  def add(graph: AccessGraph, e: AGEdge) = {
    val source = graph.getNode(e.source)
    val target = graph.getNode(e.target)
    e.kind match {

      case Contains =>
        (source.kind, source.t, target.t) match {
          case (Package, _, i: TypedKindDeclHolder) =>

            val cu = i.decl.get.compilationUnit()


            val cpath = source.containerPath.map(graph.getNode(_).name)
            val sepPath = cpath.tail.mkString(java.io.File.separator)

            val relativePath = sepPath + java.io.File.separator + target.name + ".java"
            cu.setPathName(relativePath)
            cu.setRelativeName(relativePath) // weird but seems to be the default behavior
            cu.setPackageDecl(source.fullName)

          case (Interface, th: TypedKindDeclHolder, AbstractMethodDeclHolder(smdecl)) =>
            th.decl.get.addBodyDecl(smdecl.get)

          case (Class, th: TypedKindDeclHolder, ConcreteMethodDeclHolder(smdecl)) =>
            th.decl.get.addBodyDecl(smdecl.get)


          case (Package, _, Package) => () // can be ignored

          case _ => logger.writeln("%s not created".format(e))

        }

      case Isa =>
        (source.t, target.t) match {
          case (ClassDeclHolder(sDecl), idh: InterfaceDeclHolder) =>
            sDecl.get.addImplements(idh.createLockedAccess().get)
          case _ => logger.writeln("%s not created".format(e))
        }

      case Uses =>

        (source.kind, target.kind) match {
          case (ConstructorMethod, Constructor) =>
            () //already generated when creating ConstructorMethod decl
          case (Class, Interface) =>
            logger.writeln("do not create %s : assuming its an isa edge (TOCHECK)".format(e)) // class imple

          /*case (f @ Field(), k : TypeKind) =>
          f.decl.setTypeAccess(k.lockedAccess())

        case ( m @ Method(), k : TypeKind ) =>
          m.`type`.input.types.foreach{
            case NamedType(n) =>

          }*/
          case _ => logger.writeln(" =========> need to create " + e)
        }

    }
  }


  def redirectTarget(graph: AccessGraph, e: AGEdge, newTargetId: NodeId) {
    if(e.target != newTargetId) {
      val target = graph.getNode(e.target)
      val source = graph.getNode(e.source)
      val newTarget = graph.getNode(newTargetId)
      (target.t, newTarget.t) match {
        case (InterfaceDeclHolder(Some(odlDecl)), InterfaceDeclHolder(Some(newDecl)))
          if e.kind == Isa =>
          source.t match {
            case ClassDeclHolder(Some(srcDecl)) =>
              srcDecl.replaceImplements(odlDecl.createLockedAccess(), newDecl.createLockedAccess())
            case _ => throw new JavaAGError("isa arc should only be between TypeKinds")
          }



        case (oldk: TypedKindDeclHolder, newk: TypedKindDeclHolder) =>
          source.t match {
            case FieldDeclHolder(Some(fdecl)) =>
              fdecl.replaceTypeAccess(oldk.createLockedAccess().get, newk.createLockedAccess().get)
            case ConcreteMethodDeclHolder(Some(mdecl)) =>
              mdecl.replaceTypeAccess(oldk.createLockedAccess().get, newk.createLockedAccess().get)
            case AbstractMethodDeclHolder(Some(mdecl)) =>
              mdecl.replaceTypeAccess(oldk.createLockedAccess().get, newk.createLockedAccess().get)

            case  ClassDeclHolder(_) =>
              logger.writeln("Class user of TypeKind, assume this is the \"doublon\" of " +
                "an isa arc, redirection ignored", 1)
            case k =>
              throw new JavaAGError(k + " as user of TypeKind, redirection unhandled !")
          }


        case (oldk: MethodDeclHolder, newk: MethodDeclHolder) =>
          source.t match {
            case ConstructorDeclHolder(Some(cdecl)) =>
              cdecl.replaceMethodCall(oldk.decl.get, newk.decl.get)
            case mdh: MethodDeclHolder =>
              mdh.decl.get.replaceMethodCall(oldk.decl.get, newk.decl.get)
            case k =>
              throw new JavaAGError(k + " as user of Method, redirection unhandled !")
          }



        case (ConstructorDeclHolder(_), ConstructorMethodDeclHolder(Some(newDecl), _)) =>
          source.t match {
            case ConstructorDeclHolder(_) =>
              throw new JavaAGError("redirection to constructor method within " +
                "constructor no implemented (see methodDecl)")
            case mdh: MethodDeclHolder =>
              mdh.decl.get.replaceByConstructorMethodCall(newDecl)

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


  def redirectSource(resultGraph: AccessGraph,
                     reenactor : JavaAccessGraph,
                     e: AGEdge, newSourceId: NodeId) {
    import AST.ASTNode.VIS_PUBLIC

    if (e.source != newSourceId) {
      //else do nothing*
      val source = resultGraph.getNode(e.source)
      val target = resultGraph.getNode(e.target)
      val newSource = resultGraph.getNode(newSourceId)
      (source.t, newSource.t, target.t) match {
        case (ClassDeclHolder(Some(c1Decl)), ClassDeclHolder(Some(c2Decl)),
        ConcreteMethodDeclHolder(Some(mdecl))) =>
          c1Decl.removeBodyDecl(mdecl)
          c2Decl.addBodyDecl(mdecl)
          //TODO fix : following call create a qualifier if it is null
          //the qualifier is a parameter for which a new instance is created
          //in some case it changes the meaning of the program !!
          c1Decl.replaceMethodCall(mdecl, mdecl)

          if (c1Decl.getVisibility != VIS_PUBLIC) {
            reenactor.users(e.target).find { uerId =>
              reenactor.packageNode(uerId) != reenactor.packageNode(newSourceId)
            } match {
              case Some(_) => mdecl.setVisibility(VIS_PUBLIC)
              case None =>
            }
          }

        case (PackageDeclHolder, PackageDeclHolder, i: TypedKindDeclHolder) =>
          /*println("moving typedecl %s of package (cu contains %d typedecl)".format(e.target.fullName,
            i.decl.compilationUnit().getNumTypeDecl))*/

          val idecl = i.decl.get
          //println("moving " + i +" from package "+ p1 +" to package" + p2)
          if (idecl.compilationUnit().getNumTypeDecl > 1) {
            val oldcu = idecl.compilationUnit()

            val rootPathName = oldcu.getRootPath
            oldcu.removeTypeDecl(idecl)

            val path = rootPathName + newSource.fullName.replaceAllLiterally(".", java.io.File.separator) +
              java.io.File.separator

            val newCu = new CompilationUnit()
            oldcu.programRoot().insertUnusedType(path, newSource.fullName, idecl)

            /*import scala.collection.JavaConversions.asScalaIterator
            asScalaIterator(oldcu.getImportDeclList.iterator).foreach{newCu.addImportDecl}*/
          }
          else
            idecl.compilationUnit().setPackageDecl(newSource.fullName)

          if (idecl.getVisibility != VIS_PUBLIC) {
            reenactor.users(e.target).find { userId =>
              reenactor.packageNode(userId) != newSourceId
            } match {
              case Some(_) => idecl.setVisibility(VIS_PUBLIC)
              case None => ()
            }
          }

        case (ClassDeclHolder(Some(classDecl)), InterfaceDeclHolder(Some(absDecl)), idh @ InterfaceDeclHolder(Some(superDecl))) =>
          classDecl.removeImplements(superDecl)
          absDecl.addSuperInterfaceId(idh.createLockedAccess().get)

        case (InterfaceDeclHolder(Some(subDecl)), InterfaceDeclHolder(Some(absDecl)), idh @ InterfaceDeclHolder(Some(superDecl))) =>
          subDecl.removeSuperInterface(superDecl)
          absDecl.addSuperInterfaceId(idh.createLockedAccess().get)

        case _ => throw new JavaAGError("redirecting SOURCE of %s to %s : application failure !".format(e, newSource))
      }
    }
  }
}