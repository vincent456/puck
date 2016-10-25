/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package org.extendj.concretize


import org.extendj.ast._
import puck.graph._
import puck.javaGraph.nodeKind.{TypeVariable => PuckTV, _}
import org.extendj.ast

object CreateNode {

  def apply
  ( prog : ast.Program,
    resultGraph : DependencyGraph,
    id2decl : Map[NodeId, ASTNodeLink],
    node : ConcreteNode ): Map[NodeId, ASTNodeLink] = {

    lazy val isGenericType = resultGraph.parametersOf(node.id).nonEmpty
    lazy val isGenericMethod =
      resultGraph.parametersOf(node.id).exists(n => resultGraph.kindType(n) == TypeVariableKT)

    val dh : ASTNodeLink =
      node.kind match {
        case Package => PackageDeclHolder
        case Interface =>
          createInterface(prog, resultGraph, node, isGenericType)
        case Class =>
          createClass(prog, resultGraph, node, isGenericType)
        case AbstractMethod =>
          MethodDeclHolder(createMethod(prog, resultGraph, id2decl, node,
            isGenericMethod, isAbstract = true))
        case StaticMethod =>
          MethodDeclHolder(createMethod(prog, resultGraph, id2decl, node,
            isGenericMethod, isStatic = true))
        case Method =>
          MethodDeclHolder(createMethod(prog, resultGraph, id2decl, node,
            isGenericMethod))
        case Constructor =>
          createConstructor(prog, resultGraph, id2decl, node)
        case Field => createField(prog, resultGraph, id2decl, node)
        case Param => createParameter(prog, resultGraph, id2decl, node)
        case PuckTV =>
          val tv = new GeneratedTypeVariable()
          tv setID node.name
          TypeVariableHolder(tv)
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
        mdecl setBlock block
        id2decl + (definition -> BlockHolder(block))
      case _ => id2decl
    }
  }

  def createNewInstanceExpr
  ( field : ast.FieldDeclarator,
    cdecl : ast.ConstructorDecl
  ) : Unit = {
    val expr = new ast.ClassInstanceExpr()
    expr setAccess cdecl.hostType().createLockedAccess()
    field.setInit(expr)
  }


  def createInterface
  (prog : ast.Program,
   graph : DependencyGraph,
   node : ConcreteNode,
   generic : Boolean
  ) : InterfaceDeclHolder = {
    val itc =
      if(generic) InterfaceDeclHolder(new ast.GenericInterfaceDecl())
      else InterfaceDeclHolder(new ast.InterfaceDecl())
    createTypeDecl(itc.decl, prog, graph, node)
    itc
  }

  def createClass
  ( prog : ast.Program,
    graph : DependencyGraph,
    node : ConcreteNode,
    generic : Boolean
  ) : ClassDeclHolder = {
    val cls =
      if(generic) ClassDeclHolder(new ast.GenericClassDecl())
      else ClassDeclHolder(new ast.ClassDecl())
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
    isGeneric : Boolean ,
    isAbstract : Boolean = false,
    isStatic : Boolean = false
  ) : ast.MethodDecl = {
    val decl =
      if(isGeneric) new ast.GenericMethodDecl()
      else new ast.MethodDecl()
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
