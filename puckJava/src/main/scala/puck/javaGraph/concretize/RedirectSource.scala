package puck.javaGraph.concretize

import puck.PuckError
import puck.graph._
import puck.javaGraph._
import puck.util.{PuckLog, PuckLogger}

import ShowDG._
import JavaDG2AST.verbosity

object RedirectSource {

  def apply
  ( resultGraph: DependencyGraph,
    reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    e: DGEdge, newSourceId: NodeId)
  ( implicit program : AST.Program, logger : PuckLogger) : Unit =  {
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
      logger.writeln("moving " + tDecl.fullName() +" to package" + newPackage)

      if (tDecl.compilationUnit.getNumTypeDecl > 1) {
        logger.writeln(tDecl.name + " cu with more than one classe")(verbosity(PuckLog.Debug))

        val path = ASTNodeLink.getPath(resultGraph, e.target)
        val oldcu = tDecl.compilationUnit()
        oldcu.removeTypeDecl(tDecl)
        oldcu.programRoot().insertUnusedType(path, resultGraph.fullName(newPackage.id), tDecl)

      }
      else {
        logger.writeln(tDecl.name + " cu with one classe")(verbosity(PuckLog.Debug))
        logger.writeln("before " + program.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))
        CreateEdge.setPackageDecl(resultGraph, newPackage.id, e.target, tDecl)
        logger.writeln("after " + program.getNumCompilationUnit + " cus in prog")(verbosity(PuckLog.Debug))

      }

      if (tDecl.getVisibility != VIS_PUBLIC) {
        reenactor.usersOf(e.target).find { userId =>
          reenactor.hostNameSpace(userId) != newSourceId
        } match {
          case Some(_) => tDecl.setVisibility(VIS_PUBLIC)
          case None => ()
        }
      }

      logger.writeln("tDecl.packageName() = " + tDecl.packageName())

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
          removeIsa(id2declMap(source.id),id2declMap(target.id))
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
      sub.decl.removeSuperType(sup.decl.createLockedAccess())

    case e => logger.writeln(s"isa($e) not created")
  }

}
