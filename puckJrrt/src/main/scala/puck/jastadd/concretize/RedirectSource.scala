package puck
package jastadd
package concretize

import puck.graph
import puck.graph.{TypeDecl => PTypeDecl, _}
import puck.javaGraph._
import puck.util.{PuckLog, PuckLogger}
import org.extendj.ast.{List => _, _}
import org.extendj.ast.JavaJastAddDG2AST.verbosity

import ShowDG._

object RedirectSource {


  def moveMemberDecl
  ( reenactor : DependencyGraph,
    tDeclFrom : TypeDecl,
    tDeclDest : TypeDecl,
    mDecl : BodyDecl,
    mDeclId : NodeId) : Unit = {

    tDeclFrom.removeBodyDecl(mDecl)
    tDeclDest.addBodyDecl(mDecl)

    ASTNodeLink.enlargeVisibility(
      reenactor, mDecl.asInstanceOf[Visible],
      mDeclId )
  }

  def removeImport
  ( cu: CompilationUnit, tDecl : TypeDecl)
  ( implicit logger : PuckLogger) : Unit= {
    logger.writeln(s"removeImportDecl of $tDecl in ${cu.pathName}")
    cu.removeImportDecl(tDecl)
  }
  def addImport
  ( cu: CompilationUnit, tDecl : TypeDecl)
  ( implicit logger : PuckLogger) : Unit= {
    val pa = new TypeAccess(tDecl.fullName())
    pa.lock(tDecl)
    if(!(tDecl.getVisibility == ASTNode.VIS_PUBLIC))
      tDecl.setVisibility(ASTNode.VIS_PUBLIC)
    logger.writeln(s"addImportDecl of $pa in ${cu.pathName}")
    cu.addImportDecl(new SingleTypeImportDecl(pa))
  }

  def fixImportOfMovedTypeDecl
  (reenactor : DependencyGraph,
   id2declMap: NodeId => ASTNodeLink,
   tDecl : TypeDecl,
   tDeclId : NodeId,
   oldPackage : String,
   newPackage : String,
   newlyCreatedCu : Boolean)
  ( implicit logger : PuckLogger): Unit = {

    val typesUsed = for {
      nid <- reenactor subTree tDeclId
      id <- reenactor usedBy nid
      if reenactor.kindType(id) == PTypeDecl
    } yield id

    val cu = tDecl.compilationUnit()

    def isAutoImported(t : TypeDecl) : Boolean = {
      t.isPrimitiveType || t.isString || t.isArrayDecl || t.isObject || t.isVoid
    }

    //3 cas
    //used est dans oldPackage -> ajouter import
    //used est dans newPackage -> enlever import
    //used est dans autre package -> remplacer import

    val _ = (typesUsed.toSet[NodeId] - tDeclId).foreach{
       typeId =>
          id2declMap(typeId) match {
            case TypedKindDeclHolder(t) =>
              if(!isAutoImported(t))
                t.compilationUnit().packageName() match {
                  case `newPackage` => removeImport(cu, t)
                  case `oldPackage` => addImport(cu, t)
                  case _ if newlyCreatedCu => addImport(cu, t)
                  case _ => () // should be handled by the name locking
                  //addImport(cu, tDecl); removeImport(cu, tDecl)
                }
            case NoDecl if reenactor.fullName(typeId) == "@primitive.[]" => ()
            case h =>
              error(s"TypedKindDeclHolder expected got $h for ${reenactor.fullName(typeId)}")
          }

    }
    //reenactor.containerOfKindType_!(NameSpace, )

  }


  def fixImportForUsersOfMovedTypeDecl
  ( reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    tDecl : TypeDecl,
    tDeclId : NodeId,
    oldPackage : String,
    newPackage : String)
  ( implicit logger : PuckLogger): Unit = {

    def diffTypeDecl(td : TypeDecl) =
      if(td != tDecl) Some(td.compilationUnit())
      else None

    val staticContent = reenactor.content(tDeclId) filter (id => reenactor.kindType(id) match {
      case TypeConstructor | StaticValueDecl => true
      case _ => false})

    val impactedUsers =  (staticContent flatMap reenactor.usersOf) ++ (reenactor usersOf tDeclId)

    val _ = impactedUsers.foldLeft(Set[String]()){ (cus, userId) =>
      val scu = id2declMap(userId) match {
        case ParameterDeclHolder(decl) =>
          diffTypeDecl(decl.hostType())
        case BlockHolder(blk) =>
          diffTypeDecl(blk.hostType())
        case ExprHolder(expr) =>
          diffTypeDecl(expr.hostType())
        case dh : HasBodyDecl =>
          diffTypeDecl(dh.decl.hostType())
        case tdh : TypedKindDeclHolder =>
          diffTypeDecl(tdh.decl)

        case dh => throw new DGError("should not happen, decl holder class is " + dh.getClass)
      }

      //3 cas
      //user est dans oldPackage -> ajouter import
      //user est dans newPackage -> enlever import
      //user est dans autre package -> remplacer import
      scu match {
        case Some(cu) if !cus.contains(cu.pathName()) =>

          cu.packageName() match {
            case `oldPackage` => addImport(cu, tDecl)
            case `newPackage` => removeImport(cu, tDecl)
            case _ => () // should be handled by the name locking
              //addImport(cu, tDecl); removeImport(cu, tDecl)
          }

          cus + cu.pathName
        case _ => cus
      }
    }

  }

  def moveTypeKind
  ( resultGraph: DependencyGraph,
    reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    oldPackage : NodeId,
    newPackage: NodeId,
    tDecl : TypeDecl,
    tDeclId : NodeId)
  ( implicit program : Program, logger : PuckLogger) : Unit ={
    logger.writeln("moving " + tDecl.fullName() +" to package " + resultGraph.fullName(newPackage))


    var newlyCreatedCu = false
    if(tDecl.isTopLevelType) {
      if (tDecl.compilationUnit.getNumTypeDecl > 1) {
        logger.writeln(tDecl.name + " cu with more than one classe")(verbosity(PuckLog.Debug))
        newlyCreatedCu = true
        logger.writeln(tDecl.program().prettyPrint())
        val path = ASTNodeLink.getPath(reenactor, newPackage)
        val oldcu = tDecl.compilationUnit()

        oldcu.removeTypeDecl(tDecl)
        val newCu = program.insertUnusedType(path, resultGraph.fullName(newPackage), tDecl)

        newCu.setPathName(tDecl.fullName().replaceAllLiterally(".", "/") + ".java")

      }
      else {
        logger.writeln(tDecl.name + " cu with one classe")(verbosity(PuckLog.Debug))
        logger.writeln("before " + program.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))
        CreateEdge.setPackageDecl(resultGraph, newPackage, tDeclId, tDecl)
        logger.writeln("after " + program.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))

      }
    }

    ASTNodeLink.enlargeVisibility(reenactor, tDecl, tDeclId)
    tDecl.flushCache()
    //val oldFullName = resultGraph.fullName(oldPackage) + "." + tDecl.name()
    val oldFullName = reenactor.fullName(tDeclId)
    program.changeTypeMap(oldFullName, resultGraph.fullName(tDeclId), tDecl)

    val oldPackageFullName =  reenactor.fullName(oldPackage)
    val newPackageFullName = reenactor.fullName(newPackage)
    fixImportForUsersOfMovedTypeDecl(reenactor, id2declMap, tDecl, tDeclId, oldPackageFullName, newPackageFullName)
    fixImportOfMovedTypeDecl(reenactor, id2declMap, tDecl, tDeclId, oldPackageFullName, newPackageFullName, newlyCreatedCu)

    val staticMemberType = reenactor.content(tDeclId) filter
      (nid => (reenactor kindType nid) == PTypeDecl)
    staticMemberType.foreach{ id =>
      id2declMap(id) match {
        case t : TypedKindDeclHolder =>
          moveTypeKind(resultGraph, reenactor, id2declMap, oldPackage, newPackage,t.decl, id)
        case t => error(s"TypedKindDeclHolder expected but got ${t.getClass} ")
      }

    }
  }

  def apply
  ( resultGraph: DependencyGraph,
    reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    e: DGEdge, newSourceId: NodeId)
  ( implicit program : Program, logger : PuckLogger) : Unit =  {


    def move(source : NodeId, newSource : NodeId, target : NodeId) : Unit = {
      (id2declMap(source), id2declMap(newSource), id2declMap(target)) match {
        case (ClassDeclHolder(c1Decl),
        ClassDeclHolder(c2Decl),
        bdh : HasMemberDecl) =>
          moveMemberDecl(reenactor, c1Decl, c2Decl, bdh.decl, target)

        case (InterfaceDeclHolder(oldItcDecl),
        InterfaceDeclHolder(newItcDecl),
        MethodDeclHolder(mDecl)) =>
          moveMemberDecl(reenactor, oldItcDecl, newItcDecl, mDecl, target)

        case (PackageDeclHolder, PackageDeclHolder, i: TypedKindDeclHolder) =>
          moveTypeKind(resultGraph, reenactor, id2declMap, source, newSource, i.decl, target)

        //        case (ClassDeclHolder(classDecl),
        //        InterfaceDeclHolder(absDecl),
        //        idh@InterfaceDeclHolder(superDecl)) =>
        //          classDecl.removeImplements(superDecl)
        //          absDecl.addSuperInterfaceId(idh.decl.createLockedAccess())
        //
        //        case (InterfaceDeclHolder(subDecl),
        //        InterfaceDeclHolder(absDecl),
        //        idh@InterfaceDeclHolder(superDecl)) =>
        //          subDecl.removeSuperInterface(superDecl)
        //          absDecl.addSuperInterfaceId(idh.decl.createLockedAccess())

        case _ =>
          val eStr = (reenactor, e).shows
          val nsrcStr = (reenactor, newSource).shows
          throw new JavaAGError(s"redirecting SOURCE of $eStr to $nsrcStr : application failure !")
      }
    }

    if (e.source != newSourceId) {
      //else do nothing*

      e.kind match {
        case Contains => move(e.source, newSourceId, e.target)
        case Isa =>
          removeIsa(id2declMap(e.source), id2declMap(e.target))
          CreateEdge.createIsa(id2declMap(newSourceId), id2declMap(e.target))

        case Uses => //in case of initializer
          val newSrcDecl = reenactor container_! newSourceId
          reenactor getRole newSrcDecl match {
            case Some(Initializer(_)) =>
              moveFieldInitialization(reenactor, id2declMap, e.source, newSrcDecl)
            case Some(Factory(_)) =>
              assert( reenactor getRole e.target match {
                case Some(Initializer(_)) => true
                case _ => false
              })
              moveInitFromCtorToFactory(reenactor, id2declMap, e.target, e.source, newSrcDecl)
            case sr => error(s"Redirect source of use, expecting new user to be some initializer," +
              s" ${reenactor getConcreteNode newSrcDecl} is $sr ")
          }


        case _ =>
          logger.writeln(s"Redirect source of ${e.kind} ignored")

      }
    }
  }

  def removeIsa
  ( sub : ASTNodeLink, sup : ASTNodeLink)
  ( implicit logger : PuckLogger) : Unit = (sub,sup) match {
    case (sub : TypedKindDeclHolder, sup: TypedKindDeclHolder) =>
      sub.decl.removeSuperType(sup.decl)

    case e => logger.writeln(s"isa($e) not deleted")
  }


  def moveFieldInitialization
  (reenactor : DependencyGraph,
   id2declMap: NodeId => ASTNodeLink,
   oldSource : NodeId,
   newSourceDecl : NodeId) : Unit = {

    (id2declMap(reenactor container_! oldSource),
      id2declMap(newSourceDecl)) match {
      case (FieldDeclHolder(fdecl,num), MethodDeclHolder(mdecl)) =>
       fdecl.getDeclarator(num).moveInitIntoInitializzer(mdecl)
      case hs =>
        error(s"Redirect source of use handled in case of initializer creation, $hs not expected")
    }
  }
  def moveInitFromCtorToFactory
  ( reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    initializerDeclId : NodeId,
    ctorDefId : NodeId,
    factoryDeclId : NodeId) : Unit = {

    (id2declMap(reenactor container_! ctorDefId),
      id2declMap(factoryDeclId),
      id2declMap(initializerDeclId)) match {
      case (ConstructorDeclHolder(cdecl),
      MethodDeclHolder(mdecl),
      MethodDeclHolder(init)) =>
        cdecl.removeInitCall(init)
        mdecl.createInitializerCall(init)
      case hs =>
        error(s"Redirect source of use handled in case of initializer creation, $hs not expected")
    }

  }
}
