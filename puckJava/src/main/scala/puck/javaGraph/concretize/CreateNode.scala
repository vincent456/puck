package puck.javaGraph.concretize

import puck.graph.ShowDG._
import puck.graph._
import puck.graph.constraints.DelegationAbstraction
import puck.javaGraph._
import puck.javaGraph.nodeKind._


object CreateNode {

  def apply
  ( prog : AST.Program,
    graph : DependencyGraph,
    id2decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode
    ) : ASTNodeLink = {
    node.kind match {
      case Package => PackageDeclHolder
      case Interface => createInterface(prog, graph, node)
      case Class => createClass(prog, graph, node)
      case ConstructorMethod =>
        createConstructorMethod(prog, graph, id2decl, node)
      case AbstractMethod =>
        createAbstractMethod(prog, graph, id2decl, node)
      case Constructor =>
        createConstructor(prog, graph, id2decl, node)
      case Field => createField(prog, graph, id2decl, node)
      case Param => createParameter(prog, graph, id2decl, node)

      case _ => throw new DeclarationCreationError(s"cannot create decl for kind ${node.kind}")

    }
  }

  def createNewInstanceExpr
  ( field : AST.FieldDeclaration,
    cdecl : AST.ConstructorDecl
    ) : Unit = {
    val expr = new AST.ClassInstanceExpr()
    expr.setAccess(cdecl.hostType().createLockedAccess())
    field.setInit(expr)
  }


  def createInterface
  ( prog : AST.Program,
    graph : DependencyGraph,
    node : ConcreteNode
    ) : InterfaceDeclHolder = {
    val itc = InterfaceDeclHolder(new AST.InterfaceDecl())
    createTypeDecl(itc.decl, prog, graph, node)
    itc
  }

  def createClass
  ( prog : AST.Program,
    graph : DependencyGraph,
    node : ConcreteNode
    ) : ClassDeclHolder = {
    val cls = ClassDeclHolder(new AST.ClassDecl())
    createTypeDecl(cls.decl, prog, graph, node)
    cls
  }

  def createTypeDecl
  ( decl : AST.TypeDecl,
    prog : AST.Program,
    graph : DependencyGraph,
    node : ConcreteNode) : Unit = {
    decl.setID(node.name)
    decl.setModifiers(new AST.Modifiers("public"))
    //package en path will be set when the contains arc will be add to the graph
    prog.insertUnusedType("", "", decl)
    ()
  }

  def createAbstractMethod
  ( prog : AST.Program,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode
    ) : AbstractMethodDeclHolder =
    AbstractMethodDeclHolder {
      val decl = new AST.MethodDecl()
      decl.setModifiers(new AST.Modifiers("public", "abstract"))
      decl.setID(node.name)
      decl.setTypeAccess(createTypeAccess(node.id, graph, id2Decl))
      decl
    }

  def createTypeAccess
  ( typedNode : NodeId,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink]
    ) : AST.TypeAccess = {
    graph.styp(typedNode) match {
      case Some(NamedType(tid)) =>
        id2Decl(tid) match {
          case tk: TypedKindDeclHolder =>
            tk.decl.createLockedAccess().asInstanceOf[AST.TypeAccess]
          case decl => throw new JavaAGError(s"TypedKindDeclHolder expected but got $decl")
        }
      case st => throw new JavaAGError(s"Some named type expected not but got $st")
    }
  }


  def createConstructorMethod
  ( prog : AST.Program,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode
    ) : ConstructorMethodDeclHolder = {
    val someKtor = graph.container(node.id).flatMap( graph.content(_).find{ n0 =>
      val n1 = graph.getConcreteNode(n0)
      n1.kind == Constructor &&
        graph.abstractions(n0).exists {
          case AccessAbstraction(n2 , DelegationAbstraction) => n2 == node.id
          case _ => false
        }
    })

    someKtor match {
      case None => throw new DeclarationCreationError("no constructor found")
      case Some(c) =>
        val ktor = id2Decl(c).asInstanceOf[ConstructorDeclHolder]
        val decl = ktor.decl.createConstructorMethod(node.name)
        ConstructorMethodDeclHolder(decl, ktor.decl)

    }

  }

  def createConstructor
  ( prog : AST.Program,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode
    ) : ConstructorDeclHolder = ConstructorDeclHolder {
      AST.ConstructorDecl.createConstructor(
        new AST.Modifiers("public"), node.name)
    }

  def createField
  ( prog : AST.Program,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode
    ) : FieldDeclHolder = {

    graph.styp(node.id) match {
      case Some(NamedType(id)) =>
        id2Decl get id match {
          case Some(tdh : TypedKindDeclHolder) =>
            import AST._
            val f =
              new FieldDeclaration(
                new Modifiers(ASTNode.VIS_PRIVATE),
                tdh.decl.createLockedAccess(),
                node.name)

            FieldDeclHolder(f)
          case declHolder =>
            throw new DeclarationCreationError(s"${(graph, id).shows} is not a type !! (styp = $declHolder})")
        }
      case _ => throw new DeclarationCreationError(" not a field type !!")
    }
  }

  def createParameter
  ( prog : AST.Program,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode
    ) : ParameterDeclHolder = ParameterDeclHolder {
    val ta = createTypeAccess(node.id, graph, id2Decl)
    new AST.ParameterDeclaration(new AST.Modifiers, ta,
      ta.name().toLowerCase)
  }


}
