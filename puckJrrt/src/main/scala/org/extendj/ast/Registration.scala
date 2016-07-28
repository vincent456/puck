package org.extendj.ast

import puck.graph.{ConcreteNode, NodeKind}
import puck.javaGraph.nodeKind.{EnumConstant => PuckEnumConstant, TypeVariable => PuckTypeVariable, _}

/**
  * Created by lorilan on 5/5/16.
  */
trait Registration {
  this : JastaddGraphBuilder =>

  //  val register : NodeId => ASTNode[_] =>  Unit = n => {
  //    case decl : InterfaceDecl =>
  //      register(n, Interface, InterfaceDeclHolder(decl), "InterfaceDecl")
  //    case decl : ClassDecl =>
  //      register(n, Class, ClassDeclHolder(decl), "ClassDecl")
  //    case decl : GenericInterfaceDecl =>
  //      register(n, GenericInterface, InterfaceDeclHolder(decl), "GenericInterfaceDecl")
  //    case decl : GenericClassDecl =>
  //      register(n, GenericClass, ClassDeclHolder(decl), "GenericClassDecl")
  //    case decl : ast.TypeVariable =>
  //      register(n, nodeKind.TypeVariable, TypeVariableHolder(decl), "TypeVariable")
  //    case decl : WildcardType =>
  //      register(n, WildCardType, WildCardTypeHolder(decl), "WildCardType")
  //    case decl : TypeDecl =>
  //      register(n, Primitive, PrimitiveDeclHolder(decl), "PrimitiveType")
  //    case decl : ConstructorDecl =>
  //      register(n, Constructor, ConstructorDeclHolder(decl), "ConstructorDecl")
  //    case decl : MethodDecl =>
  //      register(n, Set[NodeKind](Method, StaticMethod, AbstractMethod), MethodDeclHolder(decl) , "MethodDecl")
  //    case decl : FieldDeclaration =>
  //      register(n, Set[NodeKind](Field, StaticField), FieldDeclHolder(decl), "FieldDeclaration")
  //    case decl : ParameterDeclaration =>
  //      register(n, Param, ParameterDeclHolder(decl), "ParameterDeclaration")
  //    case decl : Expr =>
  //      register(n, Definition, ExprHolder(decl), "Expr")
  //    case decl : Block =>
  //      register(n, Definition, BlockHolder(decl), "Block")
  //    case _ =>
  //
  //  }

  private def throwRegisteringError(n : ConcreteNode, astType : String) =
    throw new Error(s"Wrong registering ! AGNode.kind : ${n.kind} while Node is an $astType")

  def register( nid : NodeIdT,
                kindExpected : JavaNodeKind,
                declHolder : => ASTNodeLink,
                kindFound : String ): Unit =
    register(nid, Set[NodeKind](kindExpected), declHolder, kindFound)

  def register( nid : NodeIdT,
                kindExpected : Set[NodeKind],
                declHolder : => ASTNodeLink,
                kindFound : String ): Unit ={
    if(kindExpected contains g.getConcreteNode(nid).kind)
      graph2ASTMap += (nid -> declHolder)
    else
      throwRegisteringError(g.getConcreteNode(nid), kindFound)
  }

  def wrapInner(tk : TypeKind, decl : ReferenceType) : TypeKind =
    if(decl.isInnerType) Inner(tk)
    else tk

  def registerDecl(n : NodeIdT, decl : InterfaceDecl) =
    register(n, wrapInner(Interface, decl), InterfaceDeclHolder(decl), "InterfaceDecl")

  def registerDecl(n : NodeIdT, decl : ClassDecl) =
    register(n, wrapInner(Class, decl), ClassDeclHolder(decl), "ClassDecl")

  def registerDecl(n : NodeIdT, decl : GenericInterfaceDecl) =
    register(n, wrapInner(GenericInterface, decl), InterfaceDeclHolder(decl), "GenericInterfaceDecl")

  def registerDecl(n : NodeIdT, decl : GenericClassDecl) =
    register(n, wrapInner(GenericClass, decl), ClassDeclHolder(decl), "GenericClassDecl")

  def registerDecl(n : NodeIdT, decl : TypeVariable) =
    register(n, PuckTypeVariable, TypeVariableHolder(decl), "TypeVariable")

  def registerDecl(n : NodeIdT, decl : AbstractWildcardType) =
    register(n, WildCardType, WildCardTypeHolder(decl), "WildCardType")

  def registerDecl(n : NodeIdT, decl : TypeDecl) =
    register(n, Primitive, PrimitiveDeclHolder(decl), "PrimitiveType")

  def registerDecl(n : NodeIdT, decl : ConstructorDecl)=
    register(n, Constructor, ConstructorDeclHolder(decl), "ConstructorDecl")

  def registerDecl(n : NodeIdT, decl : MethodDecl) : Unit =
    register(n, Set[NodeKind](Method, StaticMethod, AbstractMethod), MethodDeclHolder(decl) , "MethodDecl")

  def registerDecl(n : NodeIdT, decl : FieldDeclarator) : Unit =
    register(n, Set[NodeKind](Field, StaticField),
      FieldDeclHolder(decl.getParent.getParent().asInstanceOf[FieldDecl], decl.getChildIndex), "FieldDeclaration")

  def registerDecl(n : NodeIdT, decl : EnumConstant) : Unit =
    register(n, PuckEnumConstant, EnumConstantHolder(decl), "EnumConstant")

  def registerDecl(n : NodeIdT, decl : ParameterDeclaration) : Unit =
    register(n, Param, ParameterDeclHolder(decl), "ParameterDeclaration")

  def registerDef(n : NodeIdT, decl : Expr) : Unit =
    register(n, Definition, ExprHolder(decl), "Expr")

  def registerDef(n : NodeIdT, decl : Block) : Unit =
    register(n, Definition, BlockHolder(decl), "Block")
}
