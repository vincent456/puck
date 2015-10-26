package puck.jastadd
package concretize

import puck.graph._
import puck.javaGraph.nodeKind._


object CreateNode {

  def apply
  ( prog : AST.Program,
    resultGraph : DependencyGraph,
    id2decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode
    ) : Map[NodeId, ASTNodeLink] = {
    val dh : ASTNodeLink =
      node.kind match {
        case Package => PackageDeclHolder
        case Interface => createInterface(prog, resultGraph, node)
        case Class => createClass(prog, resultGraph, node)
        case AbstractMethod =>
          val decl = createMethod(prog, resultGraph, id2decl, node, isAbstract = true)
          AbstractMethodDeclHolder(decl)
        case StaticMethod =>
          ConcreteMethodDeclHolder(createMethod(prog, resultGraph, id2decl, node, isStatic = true))
        case Method =>
          ConcreteMethodDeclHolder(createMethod(prog, resultGraph, id2decl, node))
        case Constructor =>
          createConstructor(prog, resultGraph, id2decl, node)
        case Field => createField(prog, resultGraph, id2decl, node)
        case Param => createParameter(prog, resultGraph, id2decl, node)

        case _ => throw new DeclarationCreationError(s"cannot create decl for kind ${node.kind}")

      }
    id2decl + (node.id -> dh)

  }

  def addDef
  ( prog : AST.Program,
    resultGraph : DependencyGraph,
    id2decl : Map[NodeId, ASTNodeLink],
    container: NodeId,
    definition : NodeId) : Map[NodeId, ASTNodeLink] = {
    id2decl(container) match {
      case ConcreteMethodDeclHolder(mdecl) =>
        val block = new AST.Block()
        mdecl.setBlock(block)
        id2decl + (definition -> BlockHolder(block))
      case _ => id2decl
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

  def createMethod
  ( prog : AST.Program,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode,
    isAbstract : Boolean = false,
    isStatic : Boolean = false
    ) : AST.MethodDecl = {
    val decl = new AST.MethodDecl()
    decl.setID(node.name)
    //decl.setTypeAccess(createTypeAccess(node.id, graph, id2Decl))
    val mods = new AST.Modifiers("public")
    if(isAbstract)
      mods.addModifier("abstract")
    if(isStatic)
      mods.addModifier("static")
    decl.setModifiers(mods)
    decl
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
    id2decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode
    ) : FieldDeclHolder = {
    import AST._
    FieldDeclHolder(new FieldDeclaration(
      new Modifiers("protected"), null, node.name))
  }

  def createParameter
  ( prog : AST.Program,
    graph : DependencyGraph,
    id2decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode
    ) : ParameterDeclHolder = ParameterDeclHolder {
    //val ta = createTypeAccess(node.id, graph, id2Decl)
    new AST.ParameterDeclaration(new AST.Modifiers, null, node.name)
  }


}
