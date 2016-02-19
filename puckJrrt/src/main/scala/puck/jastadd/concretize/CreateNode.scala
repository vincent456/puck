package puck.jastadd
package concretize


import puck.graph._
import puck.javaGraph.nodeKind._
import org.extendj.ast

object CreateNode {

  def apply
  ( prog : ast.Program,
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
          MethodDeclHolder(createMethod(prog, resultGraph, id2decl, node, isAbstract = true))
        case StaticMethod =>
          MethodDeclHolder(createMethod(prog, resultGraph, id2decl, node, isStatic = true))
        case Method =>
          MethodDeclHolder(createMethod(prog, resultGraph, id2decl, node))
        case Constructor =>
          createConstructor(prog, resultGraph, id2decl, node)
        case Field => createField(prog, resultGraph, id2decl, node)
        case Param => createParameter(prog, resultGraph, id2decl, node)

        case _ => throw new DeclarationCreationError(s"cannot create decl for kind ${node.kind}")

      }
    id2decl + (node.id -> dh)

  }

  def addDef
  ( prog : ast.Program,
    resultGraph : DependencyGraph,
    id2decl : Map[NodeId, ASTNodeLink],
    container: NodeId,
    definition : NodeId) : Map[NodeId, ASTNodeLink] = {
    id2decl(container) match {
      case MethodDeclHolder(mdecl) =>
        val block = new ast.Block()
        mdecl.setBlock(block)
        id2decl + (definition -> BlockHolder(block))
      case _ => id2decl
    }
  }

  def createNewInstanceExpr
  ( field : ast.FieldDeclarator,
    cdecl : ast.ConstructorDecl
    ) : Unit = {
    val expr = new ast.ClassInstanceExpr()
    expr.setAccess(cdecl.hostType().createLockedAccess())
    field.setInit(expr)
  }


  def createInterface
  ( prog : ast.Program,
    graph : DependencyGraph,
    node : ConcreteNode
    ) : InterfaceDeclHolder = {
    val itc = InterfaceDeclHolder(new ast.InterfaceDecl())
    createTypeDecl(itc.decl, prog, graph, node)
    itc
  }

  def createClass
  ( prog : ast.Program,
    graph : DependencyGraph,
    node : ConcreteNode
    ) : ClassDeclHolder = {
    val cls = ClassDeclHolder(new ast.ClassDecl())
    createTypeDecl(cls.decl, prog, graph, node)
    cls
  }

  def createTypeDecl
  ( decl : ast.TypeDecl,
    prog : ast.Program,
    graph : DependencyGraph,
    node : ConcreteNode) : Unit = {
    decl.setID(node.name)
    decl.setModifiers(new ast.Modifiers("public"))
    //package en path will be set when the contains arc will be add to the graph
    prog.insertUnusedType("", "", decl)
    ()
  }

  def createMethod
  ( prog : ast.Program,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode,
    isAbstract : Boolean = false,
    isStatic : Boolean = false
    ) : ast.MethodDecl = {
    val decl = new ast.MethodDecl()
    decl.setID(node.name)
    //decl.setTypeAccess(createTypeAccess(node.id, graph, id2Decl))
    val mods = new ast.Modifiers("public")
    if(isAbstract)
      mods.addModifier("abstract")
    if(isStatic)
      mods.addModifier("static")
    decl.setModifiers(mods)
    decl
  }



  def createConstructor
  ( prog : ast.Program,
    graph : DependencyGraph,
    id2Decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode
    ) : ConstructorDeclHolder = ConstructorDeclHolder {
    ast.ConstructorDecl.createConstructor(
      new ast.Modifiers("public"), node.name)
  }

  def createField
  ( prog : ast.Program,
    graph : DependencyGraph,
    id2decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode
    ) : FieldDeclHolder = {
    val declarator = new ast.FieldDeclarator()
    declarator.init$Children()
    declarator.setID(node.name)

    val decl = new ast.FieldDecl()
    decl.init$Children()
    decl.setModifiers(new ast.Modifiers("protected"))
    decl.addDeclarator(declarator)

    FieldDeclHolder(decl, 0)
  }

  def createParameter
  ( prog : ast.Program,
    graph : DependencyGraph,
    id2decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode
    ) : ParameterDeclHolder = ParameterDeclHolder {
    //val ta = createTypeAccess(node.id, graph, id2Decl)
    new ast.ParameterDeclaration(new ast.Modifiers, null, node.name)
  }


}
