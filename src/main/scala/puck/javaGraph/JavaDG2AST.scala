package puck.javaGraph

import java.io.{FileReader, File}
import java.util.NoSuchElementException

import puck.PuckError
import puck.graph.DGEdge.IsaK
import puck.graph._, ShowDG._
import puck.graph.constraints.{ConstraintsParser, SupertypeAbstraction}
import puck.graph.io.{DG2ASTBuilder, DG2AST}
import puck.graph.transformations._
import puck.javaGraph.nodeKind._
import puck.util.PuckLog._
import puck.util.{PuckLog, PuckLogger}

object JavaDG2AST extends DG2ASTBuilder {
  def packageNode(graph : DependencyGraph, id : NodeId) : NodeId ={
    def aux(id : NodeId) : NodeId =
      graph.getConcreteNode(id).kind match {
        case Package => id
        case _ => aux(graph.container(id).
          getOrElse(throw new DGError( graph.fullName(id) + "has no package")))
      }
    aux(id)
  }

  def apply
  ( srcDirectory : File,
    outDirectory : File,
    jarListFile : File,
    logger : PuckLogger,
    ll : AST.LoadingListener = null
    ) : DG2AST = {
    import puck.util.FileHelper.{fileLines, findAllFiles}

    val sProg = puck.util.Time.time(logger, defaultVerbosity) {
      logger.writeln("Compiling sources ...")

      val srcSuffix = ".java"
      val sources = findAllFiles(srcDirectory, srcSuffix, outDirectory.getName)
      val jars = findAllFiles(srcDirectory, ".jar", outDirectory.getName)

      CompileHelper(sources, fileLines(jarListFile) ++: jars )
    }

    puck.util.Time.time(logger, defaultVerbosity) {
      logger.writeln("Building Access Graph ...")
      sProg match {
        case None => throw new DGBuildingError("Compilation error, no AST generated")
        case Some(p) =>
          val t = CompileHelper.buildGraph(p, ll)
          new JavaDG2AST(logger, t._1, t._2, t._3, t._4, t._5)
        }
    }

    /*val (numClass, numItc) = g.concreteNodes.foldLeft((0,0)){ case ((numClass0, numItc0), n) =>
      val numClass1 = if(n.kind == nodeKind.Class) numClass0 + 1
      else numClass0
      val numItc1 = if(n.kind == nodeKind.Interface) numItc0 + 1
      else numItc0
      (numClass1, numItc1)

    }
    logger.writeln( numClass + " classes and " + numItc + " interfaces parsed")
*/
  }
}
import JavaDG2AST._
class JavaDG2AST
(val logger : PuckLogger,
 val program : AST.Program,
 val initialGraph : DependencyGraph,
 val initialRecord : Seq[Transformation],
 val nodesByName : Map[String, NodeId],
 val graph2ASTMap : Map[NodeId, ASTNodeLink]) extends DG2AST{



  implicit val defaultVerbosity = (PuckLog.AG2AST, PuckLog.Info)

  def safeGet(graph : DependencyGraph, id2declMap : Map[NodeId, ASTNodeLink])(id :NodeId): ASTNodeLink =
  try id2declMap(id)
  catch {
    case e : NoSuchElementException =>
      val n = graph.getConcreteNode(id)
      if(n.kind == Package)
        PackageDeclHolder
      else
        NoDecl
  }

  def astNodeOf(graph : DependencyGraph, id : NodeId) : ASTNodeLink =
    safeGet(graph, graph2ASTMap)(id)

  def verbosity : PuckLog.Level => PuckLog.Verbosity = l => (PuckLog.AG2AST, l)

  def parseConstraints(decouple : File) : DG2AST  =
    try {
      //val parser = ConstraintsPlParser(nodesByName)
      val cm = ConstraintsParser(nodesByName, new FileReader(decouple))
      new JavaDG2AST(logger, program,
        initialGraph.newGraph(constraints = cm),
        initialRecord,
        nodesByName,
        graph2ASTMap)
    } catch {
      case e : Error =>
      //e.printStackTrace()
        logger.writeln("parsing failed : " + e.getMessage)((PuckLog.NoSpecialContext, PuckLog.Error))
        this
    }


  def apply(result : ResultT) : Unit = {

    logger.writeln("applying change !")
    val record = recordOfResult(result)



    record.reverse.foldLeft((graphOfResult(result), initialGraph, graph2ASTMap)) {
      case ((resultGraph, reenactor, g2AST), t : Transformation) =>

        //logger.writeln(showDG[Transformation](reenactor).shows(t))

        val res = applyOneTransformation(resultGraph, reenactor, g2AST, t)

        (resultGraph, t.redo(reenactor), res)

      case (acc, op) =>
        logger.writeln(showDG[Recordable](acc._1).shows(op))
        acc
    }

    logger.writeln("change applied : ")
    logger.writeln(program)
    program.flushCaches()
    program.eliminateLockedNamesInSources()
    logger.writeln("Program after unlock : ")
    logger.writeln(program)

  }

  def printCode(dir : File) : Unit =
    program.printCodeInDirectory(dir)

  val discardedOp : Operation => Boolean = {
    case _ : Comment
    | _ : TypeDependency => true
    case _ => false

  }

  def applyOneTransformation
  ( resultGraph : DependencyGraph,
    reenactor: DependencyGraph,
    id2declMap: Map[NodeId, ASTNodeLink],
    t: Transformation) : Map[NodeId, ASTNodeLink] = t match {
    case Transformation(Regular, CNode(n)) =>
      //redo t before createDecl
      val newMap = id2declMap get n.id match {
        case Some(_) => id2declMap
        case None =>
          val dh = ASTNodeLink.createDecl(program, resultGraph, id2declMap, n)

          id2declMap + (n.id -> dh)
      }

      newMap

    case _ =>
      lazy val noApplyMsg = s"${showDG[Recordable](resultGraph).shows(t)} not applied"

      t match {
      case Transformation(Regular, Edge(e)) =>
        //println("creating edge " + e)
        addEdge(resultGraph, safeGet(resultGraph, id2declMap), e)

      case Transformation(_, RedirectionWithMerge(_, Source(_))) =>
        logger.writeln(noApplyMsg)

      case Transformation(_, RedirectionOp(e, Source(newSource))) =>
        redirectSource(resultGraph, reenactor, safeGet(resultGraph, id2declMap), e, newSource)

      case Transformation(_, RedirectionOp(e, Target(newTarget))) =>
        redirectTarget(resultGraph, reenactor, safeGet(resultGraph, id2declMap), e, newTarget)

      // TODO see if can be performed in add node instead
      case Transformation(_, Abstraction(impl, abs, SupertypeAbstraction)) =>
        (id2declMap get impl, reenactor.getConcreteNode(abs).kind) match {
          case (Some(ConcreteMethodDeclHolder(decl)), AbstractMethod) =>
            decl.setVisibility(AST.ASTNode.VIS_PUBLIC)
          case _ => ()
        }

      case Transformation(_, Abstraction(_, _, _)) => ()

      case Transformation(Reverse, CNode(n)) =>
        id2declMap get n.id foreach {
          case dh: TypedKindDeclHolder => dh.decl.puckDelete()
          case bdh : HasBodyDecl => bdh.decl.puckDelete()
          case PackageDeclHolder => ()
          case NoDecl => throw new PuckError(noApplyMsg)
        }

      case Transformation(_, ChangeNodeName(nid, _, newName)) =>
        ASTNodeLink.setName(newName)(safeGet(reenactor,id2declMap)(nid))


      case Transformation(_, op) =>
        if( discardedOp(op) ) ()
        else logger.writeln(noApplyMsg)
    }
    id2declMap
  }



  def getPath(graph: DependencyGraph, typeDeclId : NodeId) = {
    val cpath = graph.containerPath(typeDeclId)
    val names = cpath.tail.map(graph.getConcreteNode(_).name)
    program.getRootPath + names.mkString(java.io.File.separator) +".java"
  }

  def setPackageDecl(graph: DependencyGraph,
                     packageId : NodeId,
                     typeDeclNodeId : NodeId, itc : AST.TypeDecl) = {

    val cu = itc.compilationUnit()
    val pkgDecl = graph.fullName(packageId)
    val path = getPath(graph, typeDeclNodeId)
    
    cu.setPackageDecl(pkgDecl)
    cu.setPathName(path)
    cu.setRelativeName(path)

    //!\ very important !!
    cu.flushCaches()
  }

  val removeIsa : (ASTNodeLink, ASTNodeLink) => Unit = {
    case (sub : TypedKindDeclHolder, sup: TypedKindDeclHolder) =>
      sub.decl.removeSuperType(sup.decl.createLockedAccess())

    case e => logger.writeln(s"isa($e) not created")
  }

  val addIsa : (ASTNodeLink, ASTNodeLink) => Unit = {
    case (ClassDeclHolder(sDecl), idh: InterfaceDeclHolder) =>
      sDecl.addImplements(idh.decl.createLockedAccess())

    case e => logger.writeln(s"isa($e) not created")
  }

  def addEdge
    ( graph: DependencyGraph,
      id2declMap: NodeId => ASTNodeLink,
      e: DGEdge) = {

    val source = graph.getConcreteNode(e.source)
    val target = graph.getConcreteNode(e.target)

    e.kind match {

      case DGEdge.ContainsK =>
        (source.kind, id2declMap(source.id), id2declMap(target.id)) match {
          case (Package, _, i: TypedKindDeclHolder) => setPackageDecl(graph, source.id, target.id, i.decl)

          case (Interface, th: TypedKindDeclHolder, AbstractMethodDeclHolder(mdecl)) =>
            th.decl.addBodyDecl(mdecl)

          case (Package, _, PackageDeclHolder) => () // can be ignored

          case (Class, ClassDeclHolder(clsdecl), bdHolder : HasBodyDecl) =>
            clsdecl.addBodyDecl(bdHolder.decl)

          case _ => logger.writeln(" =========> %s not created".format(e))

        }

      case DGEdge.IsaK => addIsa(id2declMap(source.id), id2declMap(target.id))

      case DGEdge.UsesK =>
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
      case DGEdge.ParameterizedUsesK =>
        logger.writeln("Creation of parameterized uses ignored")


    }
  }


  def redirectTarget(graph: DependencyGraph,
                     reenactor : DependencyGraph,
                     id2declMap: NodeId => ASTNodeLink,
                     e: DGEdge, newTargetId: NodeId) : Unit =  {
    logger.writeln("redirecting %s target to %s".format(e, newTargetId))
    if(e.target != newTargetId) {
      val target = reenactor.getConcreteNode(e.target)
      val source = reenactor.getConcreteNode(e.source)
      val newTarget = reenactor.getConcreteNode(newTargetId)


      val sourceDecl = id2declMap(source.id)

      (id2declMap(target.id), id2declMap(newTarget.id)) match {
        case (InterfaceDeclHolder(odlDecl), InterfaceDeclHolder(newDecl))
          if e.kind == IsaK =>
          sourceDecl match {
            case ClassDeclHolder(srcDecl) =>
              srcDecl.replaceImplements(odlDecl.createLockedAccess(), newDecl.createLockedAccess())
            case _ => throw new JavaAGError("isa arc should only be between TypeKinds")
          }

        case (oldk: TypedKindDeclHolder, newk: TypedKindDeclHolder) =>
          sourceDecl match {

            case mdh : HasBodyDecl =>
              mdh.decl.replaceTypeAccess(oldk.decl.createLockedAccess(), newk.decl.createLockedAccess())

            case ClassDeclHolder(_) =>
              logger.writeln("Class user of TypeKind, assume this is the \"doublon\" of " +
                "an isa arc, redirection ignored")
            case k =>
              throw new JavaAGError(k + " as user of TypeKind, redirection unhandled !")
          }



        case (oldk: ClassDeclHolder, newk: FieldDeclHolder) =>
          // assume this a case replace this.m by delegate.m
          logger.writeln()
          logger.writeln()
          logger.writeln("*** REPLACE THIS QUALIFIER")
          val t = Transformation(Regular, RedirectionOp(e, Target(newTargetId)))
          logger.writeln(showDG[Transformation](reenactor).shows(t))
          //TODO refine
          (sourceDecl, newTarget.styp) match {
            case (bdh : HasBodyDecl, Some(NamedType(fieldType))) =>
              logger.write("*** typeUse ")
              logger.writeln(showDG[DGEdge](reenactor).shows(DGEdge.UsesK(newTargetId, fieldType)))
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
              throw new JavaAGError(showDG[Transformation](reenactor).shows(t) + " unhandled")
          }


        case (FieldDeclHolder(oldDecl), FieldDeclHolder(newDecl)) =>
          sourceDecl match {
            case bdh : HasBodyDecl =>
              bdh.decl.replaceFieldAccess(oldDecl, newDecl.createLockedAccess())
            case k =>
              throw new JavaAGError(k + " as user of Field, redirection unhandled !")
          }


        case (oldk: MethodDeclHolder, newk: MethodDeclHolder) =>
          logger.writeln("replace method call !")
          sourceDecl match {
            case bdh : HasBodyDecl =>
              bdh.decl.replaceMethodCall(oldk.decl, newk.decl)
            case k =>
              throw new JavaAGError(k + " as user of Method, redirection unhandled !")
          }

        case (ConstructorDeclHolder(oldc), ConstructorDeclHolder(newc)) =>
          sourceDecl match {
            case mdh: MethodDeclHolder =>
              mdh.decl.replaceConstructorCall(oldc, newc)
            case _ =>
              throw new JavaAGError("constructor change, kind of uses source unhandled")
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
                     reenactor : DependencyGraph,
                     id2declMap: NodeId => ASTNodeLink,
                     e: DGEdge, newSourceId: NodeId) : Unit =  {
    import AST.ASTNode.VIS_PUBLIC



    def moveMemberDecl(reenactor : DependencyGraph,
                    tDeclFrom : AST.TypeDecl,
                    tDeclDest : AST.TypeDecl,
                    mDecl : AST.BodyDecl) : Unit = {

      tDeclFrom.removeBodyDecl(mDecl)
      tDeclDest.addBodyDecl(mDecl)
      //TODO fix : following call create a qualifier if it is null
      //the qualifier is a parameter for which a new instance is created
      //in some case it changes the meaning of the program !!
      //tDeclFrom.replaceMethodCall(mDecl, mDecl)

      //TODO ameliorate this !!!
      mDecl match {
        case decl: AST.MethodDecl => decl.setVisibility(VIS_PUBLIC)
        case decl: AST.FieldDeclaration => decl.setVisibility(VIS_PUBLIC)
        case _ => ()
      }
//      if (tDeclFrom.getVisibility != VIS_PUBLIC) {
//        (reenactor.usersOf(e.target).find { uerId =>
//          packageNode(reenactor, uerId) != packageNode(reenactor, newSourceId)
//        }, mDecl) match {
//          case (Some(_), decl : AST.MethodDecl) => decl.setVisibility(VIS_PUBLIC)
//          case (Some(_), decl : AST.FieldDeclaration) => decl.setVisibility(VIS_PUBLIC)
//          case (Some(_), decl) => assert(false, "MethodDecl or FieldDeclaration expected got " + decl.getClass)
//          case (None, _) => ()
//        }
//      }
    }

    def moveTypeKind(newPackage: DGNode,  tDecl : AST.TypeDecl): Unit ={
      println("moving " + tDecl.fullName() +" to package" + newPackage)

      if (tDecl.compilationUnit.getNumTypeDecl > 1) {
        logger.writeln(tDecl.name + " cu with more than one classe")(verbosity(PuckLog.Debug))

        val path = getPath(resultGraph, e.target)
        val oldcu = tDecl.compilationUnit()
        oldcu.removeTypeDecl(tDecl)
        oldcu.programRoot().insertUnusedType(path, resultGraph.fullName(newPackage.id), tDecl)

      }
      else {
        logger.writeln(tDecl.name + " cu with one classe")(verbosity(PuckLog.Debug))
        logger.writeln("before " + program.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))
        setPackageDecl(resultGraph, newPackage.id, e.target, tDecl)
        logger.writeln("after " + program.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))

      }

      if (tDecl.getVisibility != VIS_PUBLIC) {
        reenactor.usersOf(e.target).find { userId =>
          packageNode(reenactor, userId) != newSourceId
        } match {
          case Some(_) => tDecl.setVisibility(VIS_PUBLIC)
          case None => ()
        }
      }

      println("tDecl.packageName() = " + tDecl.packageName())

      reenactor.usersOf(e.target).foldLeft(Set[String]()){ (cus, userId) =>
        val scu = id2declMap(userId) match {
          case dh : HasBodyDecl =>
            if(dh.decl.hostType() != tDecl){
              Some(dh.decl.compilationUnit())
            }
            else None

          case tdh : TypedKindDeclHolder =>
            if(tdh.decl != tDecl)
              Some(tdh.decl.compilationUnit())
            else
              None
          case _ => throw new DGError("should not happen")
        }
        scu match {
          case Some(cu) if !cus.contains(cu.pathName()) =>
            if(cu.packageName() != tDecl.packageName()){
              println(" cu.packageName() = " + cu.packageName())
              val pa = new AST.TypeAccess(tDecl.fullName())
              pa.lock(tDecl)
              cu.addImportDecl(new AST.SingleTypeImportDecl(pa))
            }
            cus + cu.pathName()
          case _ => cus
        }
      }
      ()
    }

    def move(source : DGNode, newSource : DGNode, target : DGNode) : Unit = {
      (id2declMap(source.id), id2declMap(newSource.id), id2declMap(target.id)) match {
        case (ClassDeclHolder(c1Decl),
        ClassDeclHolder(c2Decl),
        bdh : HasMemberDecl) =>
          moveMemberDecl(reenactor, c1Decl, c2Decl, bdh.decl)

        case (InterfaceDeclHolder(oldItcDecl),
        InterfaceDeclHolder(newItcDecl),
        AbstractMethodDeclHolder(mDecl)) =>
          moveMemberDecl(reenactor, oldItcDecl, newItcDecl, mDecl)

        case (PackageDeclHolder, PackageDeclHolder, i: TypedKindDeclHolder) =>
          moveTypeKind(newSource, i.decl)

        case (ClassDeclHolder(classDecl),
        InterfaceDeclHolder(absDecl),
        idh@InterfaceDeclHolder(superDecl)) =>
          classDecl.removeImplements(superDecl)
          absDecl.addSuperInterfaceId(idh.decl.createLockedAccess())

        case (InterfaceDeclHolder(subDecl),
        InterfaceDeclHolder(absDecl),
        idh@InterfaceDeclHolder(superDecl)) =>
          subDecl.removeSuperInterface(superDecl)
          absDecl.addSuperInterfaceId(idh.decl.createLockedAccess())

        case _ =>
          import ShowDG._
          val eStr = showDG[DGEdge](reenactor).shows(e)
          val nsrcStr = showDG[DGNode](reenactor).shows(newSource)
          throw new JavaAGError(s"redirecting SOURCE of $eStr to $nsrcStr : application failure !")
      }
    }




    if (e.source != newSourceId) {
      //else do nothing*

      val source = resultGraph.getNode(e.source)
      val target = resultGraph.getNode(e.target)
      val newSource = resultGraph.getNode(newSourceId)

      e.kind match {
        case DGEdge.ContainsK => move(source, newSource, target)
        case DGEdge.IsaK =>
          removeIsa(id2declMap(source.id),id2declMap(target.id))
          addIsa(id2declMap(newSource.id), id2declMap(target.id))

        case DGEdge.UsesK =>
          throw new PuckError(s"redirect ${showDG[DGEdge](resultGraph).shows(e)} " +
            s"new source = ${showDG[NodeId](resultGraph).shows(newSourceId)}")

        case DGEdge.ParameterizedUsesK =>
          logger.writeln("Redirect source of parameterized uses ignored")

      }


    }
  }
}