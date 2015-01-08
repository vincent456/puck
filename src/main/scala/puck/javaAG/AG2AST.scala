package puck.javaAG

import puck.graph._
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.transformations._
import puck.javaAG.nodeKind._
import puck.util.{PuckLog, PuckLogger}

/**
 * Created by lorilan on 23/07/14.
 */
class AG2AST(val program : AST.Program,
             val logger : PuckLogger) {
  implicit val defaultVerbosity = (PuckLog.AG2AST, PuckLog.Info)
  def verbosity : PuckLog.Level => PuckLog.Verbosity = l => (PuckLog.AG2AST, l)

  def apply(resultGraph : AccessGraph,
            reenactor: JavaAccessGraph,
            t: Transformation) : (AccessGraph, AccessGraph) = t match {
    case Transformation(Add, TTNode(id, name, kind, styp, mutable, dh: DeclHolder)) =>
      //redo t before createDecl


      val n = resultGraph.getNode(id)
      val newRes = n.t.asInstanceOf[DeclHolder].createDecl(program, resultGraph, id)
      (newRes, reenactor.setNode(newRes.getNode(id)))


        //redo t after applying on code other transformations
    case `t` => t match {
      case Transformation(Add, TTEdge(e)) =>
        //println("creating edge " + e)
        add(resultGraph, e)

      case Transformation(Add, TTRedirection(e, Target(newTarget))) =>
        logger.writeln("redirecting %s target to %s".format(e, newTarget))
        redirectTarget(resultGraph, e, newTarget)

      case Transformation(Add, TTRedirection(e, Source(newSource))) =>
        redirectSource(resultGraph, reenactor, e, newSource)
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

      // TODO see if can be performed in add node instead
      case Transformation(_, TTAbstraction(impl, abs, SupertypeAbstraction)) =>
        (reenactor.getNode(impl).t, reenactor.getNode(abs).kind) match {
          case (ConcreteMethodDeclHolder(Some(decl)), AbstractMethod) =>
            decl.setVisibility(AST.ASTNode.VIS_PUBLIC)
          case _ => ()
        }

      case Transformation(_, TTAbstraction(_, _, _)) => ()

      case Transformation(Remove, TTNode(id, name, kind, styp, mutable, dh: TypedKindDeclHolder)) =>

        if (dh.decl.nonEmpty)
          dh.decl.get.puckDelete()
        else{
          val actualDh = resultGraph.getNode(id).t.asInstanceOf[TypedKindDeclHolder]
          if (actualDh.decl.nonEmpty)
             actualDh.decl.get.puckDelete()
        }


      case _ => logger.writeln("%s not applied on program".format(t))
    }
    (resultGraph, t.redo(reenactor))
  }



  def add(graph: AccessGraph, e: AGEdge) = {

    def addTypeDecl(packageAGNode : AGNode, typeDeclAGNode : AGNode, itc : TypedKindDeclHolder) = {

      val cu = itc.decl.get.compilationUnit()

      val cpath = packageAGNode.containerPath.map(graph.getNode(_).name)
      val sepPath = cpath.tail.mkString(java.io.File.separator)

      val relativePath = sepPath + java.io.File.separator + typeDeclAGNode.name + ".java"
      cu.setPathName(relativePath)
      cu.setRelativeName(relativePath) // weird but seems to be the default behavior
      cu.setPackageDecl(packageAGNode.fullName)
    }

    val source = graph.getNode(e.source)
    val target = graph.getNode(e.target)

    e.kind match {

      case Contains =>
        (source.kind, source.t, target.t) match {
          case (Package, _, i: TypedKindDeclHolder) => addTypeDecl(source, target, i)

          case (Interface, th: TypedKindDeclHolder, AbstractMethodDeclHolder(smdecl)) =>
            th.decl.get.addBodyDecl(smdecl.get)

          case (Class, th: TypedKindDeclHolder, mdh : MethodDeclHolder) =>
          //ConcreteMethodDeclHolder(smdecl)) =>
            th.decl.get.addBodyDecl(mdh.decl.get)


          case (Package, _, PackageDeclHolder) => () // can be ignored

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
            /*case dh @ (FieldDeclHolder(_)
              | ConcreteMethodDeclHolder(_)
              | AbstractMethodDeclHolder(_)) =>
                dh.asInstanceOf[DeclHolder].decl.map {
                _.replaceTypeAccess(oldk.createLockedAccess().get, newk.createLockedAccess().get)
              }*/

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

    def moveMethod( reenactor : JavaAccessGraph,
                    tDeclFrom : AST.TypeDecl,
                    tDeclDest : AST.TypeDecl,
                    mDecl : AST.MethodDecl) : Unit ={
      tDeclFrom.removeBodyDecl(mDecl)
      tDeclDest.addBodyDecl(mDecl)
      //TODO fix : following call create a qualifier if it is null
      //the qualifier is a parameter for which a new instance is created
      //in some case it changes the meaning of the program !!
      tDeclFrom.replaceMethodCall(mDecl, mDecl)

      if (tDeclFrom.getVisibility != VIS_PUBLIC) {
        reenactor.users(e.target).find { uerId =>
          reenactor.packageNode(uerId) != reenactor.packageNode(newSourceId)
        } match {
          case Some(_) => mDecl.setVisibility(VIS_PUBLIC)
          case None => ()
        }
      }
    }

    def moveTypeKind(newPackage: AGNode,  tDecl : AST.TypeDecl): Unit ={
      //println("moving " + i +" from package "+ p1 +" to package" + p2)
      val oldcu = tDecl.compilationUnit()

      val rootPathName = oldcu.getRootPath

      val path = rootPathName +  java.io.File.separator + newPackage.fullName.replaceAllLiterally(".", java.io.File.separator) +
        java.io.File.separator

      if (tDecl.compilationUnit.getNumTypeDecl > 1) {
        logger.writeln(tDecl.name + " cu with more than one classe")(verbosity(PuckLog.Debug))
        oldcu.removeTypeDecl(tDecl)
        val newCu = new AST.CompilationUnit()
        oldcu.programRoot().insertUnusedType(path, newPackage.fullName, tDecl)

        /*import scala.collection.JavaConvers ions.asScalaIterator
        asScalaIterator(oldcu.getImportDeclList.iterator).foreach{newCu.addImportDecl}*/
      }
      else {
        logger.writeln(tDecl.name + " cu with one classe")(verbosity(PuckLog.Debug))
        val p: AST.Program = tDecl.programRoot
        logger.writeln("before " + p.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))
        tDecl.compilationUnit.setPackageDecl(newPackage.fullName)
        tDecl.compilationUnit.setPathName(path + tDecl.name + ".java")
        logger.writeln("after " + p.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))

      }

      if (tDecl.getVisibility != VIS_PUBLIC) {
        reenactor.users(e.target).find { userId =>
          reenactor.packageNode(userId) != newSourceId
        } match {
          case Some(_) => tDecl.setVisibility(VIS_PUBLIC)
          case None => ()
        }
      }
    }

    if (e.source != newSourceId) {
      //else do nothing*
      val source = resultGraph.getNode(e.source)

      val target = resultGraph.getNode(e.target)
      val newSource = resultGraph.getNode(newSourceId)
      (source.t, newSource.t, target.t) match {
        case (ClassDeclHolder(Some(c1Decl)),
        ClassDeclHolder(Some(c2Decl)),
        ConcreteMethodDeclHolder(Some(mdecl))) =>
          moveMethod(reenactor, c1Decl, c2Decl, mdecl)

        case (InterfaceDeclHolder(Some(oldItcDecl)),
        InterfaceDeclHolder(Some(newItcDecl)),
        AbstractMethodDeclHolder(Some(mDecl))) =>
          moveMethod(reenactor, oldItcDecl, newItcDecl, mDecl)

        case (PackageDeclHolder, PackageDeclHolder, i: TypedKindDeclHolder) =>
          moveTypeKind(newSource, i.decl.get)

        case (ClassDeclHolder(Some(classDecl)),
                InterfaceDeclHolder(Some(absDecl)),
                idh @ InterfaceDeclHolder(Some(superDecl))) =>
          classDecl.removeImplements(superDecl)
          absDecl.addSuperInterfaceId(idh.createLockedAccess().get)

        case (InterfaceDeclHolder(Some(subDecl)),
                InterfaceDeclHolder(Some(absDecl)),
                idh @ InterfaceDeclHolder(Some(superDecl))) =>
          subDecl.removeSuperInterface(superDecl)
          absDecl.addSuperInterfaceId(idh.createLockedAccess().get)


        case _ => throw new JavaAGError("redirecting SOURCE of %s to %s : application failure !".format(e, newSource))
      }
    }
  }
}