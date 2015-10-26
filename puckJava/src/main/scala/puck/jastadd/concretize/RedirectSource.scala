package puck.jastadd
package concretize

import puck.PuckError
import puck.graph._
import puck.jastadd.JavaDG2AST
import puck.javaGraph._
import puck.javaGraph.nodeKind.{Method, Field}
import puck.util.{PuckLog, PuckLogger}

import ShowDG._
import JavaDG2AST.verbosity

object RedirectSource {


  def moveMemberDecl
  ( reenactor : DependencyGraph,
    tDeclFrom : AST.TypeDecl,
    tDeclDest : AST.TypeDecl,
    mDecl : AST.BodyDecl,
    mDeclId : NodeId) : Unit = {

    tDeclFrom.removeBodyDecl(mDecl)
    tDeclDest.addBodyDecl(mDecl)

    ASTNodeLink.enlargeVisibility(
      reenactor, mDecl.asInstanceOf[AST.Visible],
      mDeclId )
  }

  def addImportDeclIfNeeded
  ( reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    tDecl : AST.TypeDecl,
    tDeclId : NodeId )
  ( implicit logger : PuckLogger): Unit = {

    def diffTypeDecl(td : AST.TypeDecl) =
      if(td != tDecl) Some(td.compilationUnit())
      else None

    val _ = reenactor.usersOf(tDeclId).foldLeft(Set[String]()){ (cus, userId) =>
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
      scu match {
        case Some(cu) if !cus.contains(cu.pathName()) =>
          if(cu.packageName() != tDecl.packageName()){
            val pa = new AST.TypeAccess(tDecl.fullName())
            pa.lock(tDecl)

            logger.writeln(s"addImportDecl of $pa in ${cu.pathName}")
            cu.addImportDecl(new AST.SingleTypeImportDecl(pa))
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
    newPackage: NodeId,
    tDecl : AST.TypeDecl,
    tDeclId : NodeId)
  ( implicit program : AST.Program, logger : PuckLogger) : Unit ={
    logger.writeln("moving " + tDecl.fullName() +" to package" + resultGraph.fullName(newPackage))

    if (tDecl.compilationUnit.getNumTypeDecl > 1) {
      logger.writeln(tDecl.name + " cu with more than one classe")(verbosity(PuckLog.Debug))

      val path = ASTNodeLink.getPath(resultGraph, tDeclId)
      val oldcu = tDecl.compilationUnit()
      oldcu.removeTypeDecl(tDecl)
      val newCu = oldcu.programRoot().insertUnusedType(path, resultGraph.fullName(newPackage), tDecl)

      if(newCu.relativeName() == null){
        newCu.setRelativeName(tDecl.fullName().replaceAllLiterally(".","/") +".java")
      }
      newCu.setID(tDecl.name())

    }
    else {
      logger.writeln(tDecl.name + " cu with one classe")(verbosity(PuckLog.Debug))
      logger.writeln("before " + program.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))
      CreateEdge.setPackageDecl(resultGraph, newPackage, tDeclId, tDecl)
      logger.writeln("after " + program.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))

    }
    ASTNodeLink.enlargeVisibility(reenactor, tDecl, tDeclId)
    tDecl.flushCache()
    logger.writeln("tDecl.packageName() = " + tDecl.packageName())

    addImportDeclIfNeeded(reenactor, id2declMap, tDecl, tDeclId)
  }

  def apply
  ( resultGraph: DependencyGraph,
    reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    e: DGEdge, newSourceId: NodeId)
  ( implicit program : AST.Program, logger : PuckLogger) : Unit =  {


    def move(source : NodeId, newSource : NodeId, target : NodeId) : Unit = {
      (id2declMap(source), id2declMap(newSource), id2declMap(target)) match {
        case (ClassDeclHolder(c1Decl),
        ClassDeclHolder(c2Decl),
        bdh : HasMemberDecl) =>
          moveMemberDecl(reenactor, c1Decl, c2Decl, bdh.decl, target)

        case (InterfaceDeclHolder(oldItcDecl),
        InterfaceDeclHolder(newItcDecl),
        AbstractMethodDeclHolder(mDecl)) =>
          moveMemberDecl(reenactor, oldItcDecl, newItcDecl, mDecl, target)

        case (PackageDeclHolder, PackageDeclHolder, i: TypedKindDeclHolder) =>
          moveTypeKind(resultGraph, reenactor, id2declMap, newSource, i.decl, target)

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
      case (FieldDeclHolder(fdecl), ConcreteMethodDeclHolder(mdecl)) =>
       fdecl.moveInitIntoInitializzer(mdecl)
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
      ConcreteMethodDeclHolder(mdecl),
      ConcreteMethodDeclHolder(init)) =>
        cdecl.removeInitCall(init)
        mdecl.createInitializerCall(init)
      case hs =>
        error(s"Redirect source of use handled in case of initializer creation, $hs not expected")
    }

  }
}
