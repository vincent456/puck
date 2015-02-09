package puck.javaAG

import java.util.NoSuchElementException

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

  def safeGet(graph : DependencyGraph, id2declMap : Map[NodeId, DeclHolder])(id :NodeId): DeclHolder =
  try id2declMap(id)
  catch {
    case e : NoSuchElementException =>
      val n = graph.getNode(id)
      if(n.kind == Package)
        PackageDeclHolder
      else
        throw e
  }

  def verbosity : PuckLog.Level => PuckLog.Verbosity = l => (PuckLog.AG2AST, l)

  def apply(resultGraph : DependencyGraph,
            reenactor: JavaDependencyGraph,
            id2declMap: Map[NodeId, DeclHolder],
            t: Transformation) : Map[NodeId, DeclHolder] = t match {
    case Transformation(Add, TTNode(id, name, kind, styp, mutable)) =>
      //redo t before createDecl
      val n = resultGraph.getNode(id)
      
      val newMap = id2declMap get id match {
        case Some(_) => id2declMap
        case None =>
          val node = new DGNode(id, name, kind, styp, mutable, puck.graph.Created)
          val dh = DeclHolder.createDecl(program, resultGraph, id2declMap, node)

          id2declMap + (id -> dh)
      }

      newMap

        //redo t after applying on code other transformations
    case `t` => t match {
      case Transformation(Add, TTEdge(e)) =>
        //println("creating edge " + e)
        add(resultGraph, safeGet(resultGraph, id2declMap), e)

      case Transformation(Add, TTRedirection(e, Target(newTarget))) =>
        logger.writeln("redirecting %s target to %s".format(e, newTarget))
        redirectTarget(resultGraph, safeGet(resultGraph, id2declMap), e, newTarget)

      case Transformation(Add, TTRedirection(e, Source(newSource))) =>
        redirectSource(resultGraph, reenactor, safeGet(resultGraph, id2declMap), e, newSource)
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
        (id2declMap get impl, reenactor.getNode(abs).kind) match {
          case (Some(ConcreteMethodDeclHolder(decl)), AbstractMethod) =>
            decl.setVisibility(AST.ASTNode.VIS_PUBLIC)
          case _ => ()
        }

      case Transformation(_, TTAbstraction(_, _, _)) => ()

      case Transformation(Remove, TTNode(id, name, kind, styp, mutable)) =>
        //val
        id2declMap get id map {
          case  dh: TypedKindDeclHolder => dh.decl.puckDelete()
          case _ => logger.writeln("%s not applied on program".format(t))
        }

      case _ => logger.writeln(s"$t not applied on program")
    }
    id2declMap
  }



  def add(graph: DependencyGraph,
          id2declMap: NodeId => DeclHolder,
          e: DGEdge) = {

    def setPackageDecl(packageAGNode : DGNode, typeDeclAGNode : DGNode, itc : TypedKindDeclHolder) = {

      val cpath = graph.containerPath(typeDeclAGNode.id)

      val names = cpath.tail.map(graph.getNode(_).name)

      val cu = itc.decl.compilationUnit()

      cu.setPathName(program.getRootPath + names.mkString(java.io.File.separator) +".java")
      //cu.setRelativeName(relativePath) // weird but seems to be the default behavior
      cu.setPackageDecl(graph.fullName(packageAGNode.id))

    }

    val source = graph.getNode(e.source)
    val target = graph.getNode(e.target)

    e.kind match {

      case Contains =>
        (source.kind, id2declMap(source.id), id2declMap(target.id)) match {
          case (Package, _, i: TypedKindDeclHolder) => setPackageDecl(source, target, i)

          case (Interface, th: TypedKindDeclHolder, AbstractMethodDeclHolder(mdecl)) =>
            th.decl.addBodyDecl(mdecl)

          case (Class, th: TypedKindDeclHolder, mdh : MethodDeclHolder) =>
            th.decl.addBodyDecl(mdh.decl)


          case (Package, _, PackageDeclHolder) => () // can be ignored

          case _ => logger.writeln("%s not created".format(e))

        }

      case Isa =>
        (id2declMap(source.id), id2declMap(target.id)) match {
          case (ClassDeclHolder(sDecl), idh: InterfaceDeclHolder) =>
            sDecl.addImplements(idh.decl.createLockedAccess())


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


  def redirectTarget(graph: DependencyGraph,
                     id2declMap: NodeId => DeclHolder,
                     e: DGEdge, newTargetId: NodeId) {

    if(e.target != newTargetId) {
      val target = graph.getNode(e.target)
      val source = graph.getNode(e.source)
      val newTarget = graph.getNode(newTargetId)


      val sourceDecl = id2declMap(source.id)

      (id2declMap(target.id), id2declMap(newTarget.id)) match {
        case (InterfaceDeclHolder(odlDecl), InterfaceDeclHolder(newDecl))
          if e.kind == Isa =>
          sourceDecl match {
            case ClassDeclHolder(srcDecl) =>
              srcDecl.replaceImplements(odlDecl.createLockedAccess(), newDecl.createLockedAccess())
            case _ => throw new JavaAGError("isa arc should only be between TypeKinds")
          }



        case (oldk: TypedKindDeclHolder, newk: TypedKindDeclHolder) =>
          sourceDecl match {
            /*case dh @ (FieldDeclHolder(_)
              | ConcreteMethodDeclHolder(_)
              | AbstractMethodDeclHolder(_)) =>
                dh.asInstanceOf[DeclHolder].decl.map {
                _.replaceTypeAccess(oldk.createLockedAccess().get, newk.createLockedAccess().get)
              }*/

            case FieldDeclHolder(fdecl) =>
              fdecl.replaceTypeAccess(oldk.decl.createLockedAccess(), newk.decl.createLockedAccess())
            case ConcreteMethodDeclHolder(mdecl) =>
              mdecl.replaceTypeAccess(oldk.decl.createLockedAccess(), newk.decl.createLockedAccess())
            case AbstractMethodDeclHolder(mdecl) =>
              mdecl.replaceTypeAccess(oldk.decl.createLockedAccess(), newk.decl.createLockedAccess())

            case  ClassDeclHolder(_) =>
              logger.writeln("Class user of TypeKind, assume this is the \"doublon\" of " +
                "an isa arc, redirection ignored", 1)
            case k =>
              throw new JavaAGError(k + " as user of TypeKind, redirection unhandled !")
          }


        case (oldk: MethodDeclHolder, newk: MethodDeclHolder) =>
          sourceDecl match {
            case ConstructorDeclHolder(cdecl) =>
              cdecl.replaceMethodCall(oldk.decl, newk.decl)
            case mdh: MethodDeclHolder =>
              mdh.decl.replaceMethodCall(oldk.decl, newk.decl)
            case k =>
              throw new JavaAGError(k + " as user of Method, redirection unhandled !")
          }



        case (ConstructorDeclHolder(_), ConstructorMethodDeclHolder(newDecl, _)) =>
          sourceDecl match {
            case ConstructorDeclHolder(_) =>
              throw new JavaAGError("redirection to constructor method within " +
                "constructor no implemented (see methodDecl)")
            case mdh: MethodDeclHolder => mdh.decl.replaceByConstructorMethodCall(newDecl)

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


  def redirectSource(resultGraph: DependencyGraph,
                     reenactor : JavaDependencyGraph,
                     id2declMap: NodeId => DeclHolder,
                     e: DGEdge, newSourceId: NodeId) {
    import AST.ASTNode.VIS_PUBLIC

    def moveMethod( reenactor : JavaDependencyGraph,
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

    def moveTypeKind(newPackage: DGNode,  tDecl : AST.TypeDecl): Unit ={
      //println("moving " + i +" from package "+ p1 +" to package" + p2)
      val oldcu = tDecl.compilationUnit()

      val rootPathName = oldcu.getRootPath

      val path = rootPathName +  java.io.File.separator + resultGraph.fullName(newPackage.id).replaceAllLiterally(".", java.io.File.separator) +
        java.io.File.separator

      if (tDecl.compilationUnit.getNumTypeDecl > 1) {
        logger.writeln(tDecl.name + " cu with more than one classe")(verbosity(PuckLog.Debug))
        oldcu.removeTypeDecl(tDecl)
        oldcu.programRoot().insertUnusedType(path, resultGraph.fullName(newPackage.id), tDecl)

        /*import scala.collection.JavaConvers ions.asScalaIterator
        asScalaIterator(oldcu.getImportDeclList.iterator).foreach{newCu.addImportDecl}*/
      }
      else {
        logger.writeln(tDecl.name + " cu with one classe")(verbosity(PuckLog.Debug))
        val p: AST.Program = tDecl.programRoot
        logger.writeln("before " + p.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))
        tDecl.compilationUnit.setPackageDecl(resultGraph.fullName(newPackage.id))
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

      (id2declMap(source.id), id2declMap(newSource.id), id2declMap(target.id)) match {
        case (ClassDeclHolder(c1Decl),
        ClassDeclHolder(c2Decl),
        ConcreteMethodDeclHolder(mdecl)) =>
          moveMethod(reenactor, c1Decl, c2Decl, mdecl)

        case (InterfaceDeclHolder(oldItcDecl),
        InterfaceDeclHolder(newItcDecl),
        AbstractMethodDeclHolder(mDecl)) =>
          moveMethod(reenactor, oldItcDecl, newItcDecl, mDecl)

        case (PackageDeclHolder, PackageDeclHolder, i: TypedKindDeclHolder) =>
          moveTypeKind(newSource, i.decl)

        case (ClassDeclHolder(classDecl),
                InterfaceDeclHolder(absDecl),
                idh @ InterfaceDeclHolder(superDecl)) =>
          classDecl.removeImplements(superDecl)
          absDecl.addSuperInterfaceId(idh.decl.createLockedAccess())

        case (InterfaceDeclHolder(subDecl),
                InterfaceDeclHolder(absDecl),
                idh @ InterfaceDeclHolder(superDecl)) =>
          subDecl.removeSuperInterface(superDecl)
          absDecl.addSuperInterfaceId(idh.decl.createLockedAccess())


        case _ => throw new JavaAGError("redirecting SOURCE of %s to %s : application failure !".format(e, newSource))
      }
    }
  }
}