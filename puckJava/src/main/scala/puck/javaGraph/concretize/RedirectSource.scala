package puck.javaGraph.concretize

import puck.PuckError
import puck.graph._
import puck.javaGraph._
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
    newPackage: DGNode,
    tDecl : AST.TypeDecl,
    tDeclId : NodeId)
  ( implicit program : AST.Program, logger : PuckLogger) : Unit ={
    logger.writeln("moving " + tDecl.fullName() +" to package" + newPackage)

    if (tDecl.compilationUnit.getNumTypeDecl > 1) {
      logger.writeln(tDecl.name + " cu with more than one classe")(verbosity(PuckLog.Debug))

      val path = ASTNodeLink.getPath(resultGraph, tDeclId)
      val oldcu = tDecl.compilationUnit()
      oldcu.removeTypeDecl(tDecl)
      val newCu = oldcu.programRoot().insertUnusedType(path, resultGraph.fullName(newPackage.id), tDecl)

      if(newCu.relativeName() == null){
        newCu.setRelativeName(tDecl.fullName().replaceAllLiterally(".","/") +".java")
      }
      newCu.setID(tDecl.name())

    }
    else {
      logger.writeln(tDecl.name + " cu with one classe")(verbosity(PuckLog.Debug))
      logger.writeln("before " + program.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))
      CreateEdge.setPackageDecl(resultGraph, newPackage.id, tDeclId, tDecl)
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


    def move(source : DGNode, newSource : DGNode, target : DGNode) : Unit = {
      (id2declMap(source.id), id2declMap(newSource.id), id2declMap(target.id)) match {
        case (ClassDeclHolder(c1Decl),
        ClassDeclHolder(c2Decl),
        bdh : HasMemberDecl) =>
          moveMemberDecl(reenactor, c1Decl, c2Decl, bdh.decl, target.id)

        case (InterfaceDeclHolder(oldItcDecl),
        InterfaceDeclHolder(newItcDecl),
        AbstractMethodDeclHolder(mDecl)) =>
          moveMemberDecl(reenactor, oldItcDecl, newItcDecl, mDecl, target.id)

        case (PackageDeclHolder, PackageDeclHolder, i: TypedKindDeclHolder) =>
          moveTypeKind(resultGraph, reenactor, id2declMap, newSource, i.decl, target.id)

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

      val source = resultGraph.getNode(e.source)
      val target = resultGraph.getNode(e.target)
      val newSource = resultGraph.getNode(newSourceId)

      e.kind match {
        case Contains => move(source, newSource, target)
        case Isa =>
          removeIsa(id2declMap(source.id), id2declMap(target.id))
          CreateEdge.createIsa(id2declMap(newSource.id), id2declMap(target.id))

        case Uses =>
          throw new PuckError(s"redirect ${(resultGraph, e).shows} " +
            s"new source = ${(resultGraph, newSourceId).shows}")

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

}
