package puck.javaGraph.concretize

import puck.graph._
import puck.javaGraph._
import puck.javaGraph.nodeKind._
import puck.util.PuckLogger

object CreateEdge {

  def apply
  ( graph: DependencyGraph,
    reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    e: DGEdge)
  ( implicit program : AST.Program, logger : PuckLogger) = {
        e match {
          case c : Contains =>
            createContains(graph,reenactor, id2declMap, c)
          case _ : ContainsParam =>
            (id2declMap(e.container), id2declMap(e.content)) match {
              case (MethodDeclHolder(mdecl), ParameterDeclHolder(pdecl)) =>
                mdecl.prependParameter(pdecl)
              case (ConstructorDeclHolder(cdecl), ParameterDeclHolder(pdecl)) =>
                cdecl.prependParameter(pdecl)
              case _ =>
                error(s"ContainsParam(${reenactor.getNode(e.container)}, ${reenactor.getNode(e.content)}) " +
                  "should be between a decl and a param")
            }

          case _ : Isa =>
            createIsa(id2declMap(e.subType), id2declMap(e.superType))

          case u : Uses =>
            val (source, target ) = (graph.getNode(e.source), graph.getNode(e.target))
            (source.kind, target.kind) match {
              case (Class, Interface) =>
                logger.writeln("do not create %s : assuming its an isa edge (TOCHECK)".format(e)) // class imple
              case (ConstructorMethod, Constructor) =>
                () //already generated when creating ConstructorMethod decl
              case (Definition, Constructor) =>
                createUsesofConstructor(graph, reenactor, id2declMap, u)
              case (Definition, Field) =>
                createUsesofField(graph, reenactor, id2declMap, u)

              case _ => logger.writeln(" =========> need to create " + e)
            }
          case _  =>
            logger.writeln(s"Creation of ${e.kind} ignored")

        }

  }


  def createContains
  ( graph: DependencyGraph,
    reenactor : DependencyGraph,
    id2declMap : NodeId => ASTNodeLink,
    e : Contains)
  ( implicit program : AST.Program, logger : PuckLogger) : Unit =
    (id2declMap(e.container), id2declMap(e.content)) match {
      case (PackageDeclHolder, i: TypedKindDeclHolder) =>
        setPackageDecl(reenactor, e.container, e.content, i.decl)
      case (th: TypedKindDeclHolder, AbstractMethodDeclHolder(mdecl)) =>
        th.decl.addBodyDecl(mdecl)

      case (_, PackageDeclHolder) => () // can be ignored

      case (ClassDeclHolder(clsdecl), bdHolder : HasBodyDecl) =>
        clsdecl.addBodyDecl(bdHolder.decl)

      case _ => logger.writeln(" =========> %s not created".format(e))

    }

  def createIsa
  (sub : ASTNodeLink, sup : ASTNodeLink)
  ( implicit logger : PuckLogger) : Unit = (sub, sup) match {
    case (ClassDeclHolder(sDecl), idh: InterfaceDeclHolder) =>
      sDecl.addImplements(idh.decl.createLockedAccess())

    case e => logger.writeln(s"isa($e) not created")
  }

  def createUsesofConstructor
  ( graph: DependencyGraph,
    reenactor : DependencyGraph,
    id2declMap : NodeId => ASTNodeLink,
    e : Uses)
  ( implicit logger : PuckLogger) : Unit = {
    val sourceDecl = reenactor.container_!(e.user)
    (id2declMap(sourceDecl), id2declMap(e.used)) match {
      case (FieldDeclHolder(fdecl), ConstructorDeclHolder(cdecl))
        if fdecl.getInitOpt.isEmpty =>
        CreateNode.createNewInstanceExpr(fdecl, cdecl)
      case (_: HasBodyDecl, ConstructorDeclHolder(cdecl)) => ???
      case _ => ???

    }
  }

  def createUsesofField
  ( graph: DependencyGraph,
    reenactor : DependencyGraph,
    id2declMap : NodeId => ASTNodeLink,
    e : Uses)
  ( implicit logger : PuckLogger) : Unit = {

    val typesUsed = reenactor.usedBy(e.used).filter{
      id => reenactor.kindType(id) == TypeDecl
    }

    if (typesUsed.size != 1)
      throw new puck.graph.Error(s"require ONE type use got ${typesUsed.size}")

    val typeUse = Uses(e.used, typesUsed.head)
    val tmUses = reenactor.typeMemberUsesOf(typeUse).filter{_.user == e.user}

    (id2declMap(e.user), id2declMap(e.used)) match {
      case (dh: DefHolder, FieldDeclHolder(newReceiverDecl)) =>
        val receiver = newReceiverDecl.createLockedAccess()
        tmUses.map { u =>
          id2declMap(u.used)}.foreach {
          case MethodDeclHolder(methUsedDecl) =>
            dh.node.addNewReceiver(methUsedDecl, receiver)
          case FieldDeclHolder(fieldUsedDecl) =>
            dh.node.addNewReceiver(fieldUsedDecl, receiver)
          case used =>
            logger.writeln(s"create receiver for $used ignored")
        }

      case _ => throw new puck.graph.Error(s"method decl and field decl expected")
    }



  }


  def setPackageDecl
  ( graph: DependencyGraph,
    packageId : NodeId,
    typeDeclNodeId : NodeId,
    itc : AST.TypeDecl)
  ( implicit program : AST.Program, logger : PuckLogger) = {

    val cu = itc.compilationUnit()
    val pkgDecl = graph.fullName(packageId)
    val path = ASTNodeLink.getPath(graph, typeDeclNodeId)

    cu.setPackageDecl(pkgDecl)
    cu.setPathName(path)
    cu.setRelativeName(path)

    println("on setPackageDecl cu.packageName = " + cu.packageName())
    println("tdecl.fullName() = " + itc.fullName())
    //!\ very important !!
    cu.flushCaches()
  }

}
