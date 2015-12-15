package puck.jastadd

import puck.graph._
import puck.javaGraph.JavaGraphBuilder
import puck.javaGraph.nodeKind._
import org.extendj.{ast => AST}

import scala.collection.JavaConversions.collectionAsScalaIterable

object JastaddGraphBuilder {

  //JavaAccessor

  def isa(n1 : NodeId, n2 : NodeId) =
      new DGEdge(Isa, n1, n2)

  def classKind = Class
  def interfaceKind = Interface

  def field = Field
  def staticField = StaticField
  def constructor = Constructor
  def abstractMethod = AbstractMethod
  def method = Method
  def staticMethod = StaticMethod

  def parameter = Param

  def definition = Definition

  def primitive = Primitive
  def typeVariable = TypeVariable
  def wildcardType = WildCardType
}

class JastaddGraphBuilder(val program : AST.Program) extends JavaGraphBuilder {
  var graph2ASTMap = Map[Int, ASTNodeLink]()

  def findTypeDecl(typ : String): AST.TypeDecl ={
    val td = program findType typ
    if(td == null)
      throw new DGBuildingError(typ + " not found")
    td
  }

  def attachOrphanNodes(fromId : Int = g.rootId) : Unit = {
    val lastId = g.numNodes - 1
    if(fromId < lastId){
      for(nodeId <- Range.inclusive(fromId, lastId) ){
        //println(s"${g.container(nodeId)} contains $nodeId")
        if(g.container(nodeId).isEmpty && nodeId != g.rootId){
          val n = g.getNode(nodeId)
          //println(s"orphan node : ${n.fullName}  : ${n.kind} - container = ${n.container}")
          graph2ASTMap get nodeId match {
            case Some(FieldDeclHolder(d)) => addBodyDecl(d)
            case Some(mdh : MethodDeclHolder) => addBodyDecl(mdh.decl)
            case Some(ConstructorDeclHolder(cdecl)) => addBodyDecl(cdecl)
            case Some(tdh : TypedKindDeclHolder) => addApiTypeNode(tdh.decl)
            case sdh =>
              println( g.fullName(nodeId) + " " + sdh + " attach orphan nodes unhandled case")
              ()
          }
        }
      }
      //addBodyDecl (case MethodDeclHolder) can add new typeNodes
      attachOrphanNodes(lastId)
    }
  }

  import scala.collection.JavaConversions.asScalaBuffer
  def addParams(decl : NodeId, params : java.util.ArrayList[Integer]) : Unit =
    addParams(decl, params.toList.map(_.toInt))

  def addApiNode(nodeKind : String, typ : String, bodydeclName: String) : Unit = {
    //println("trying to add type "+typ + " " + bodydeclName+ " ... ")

    val td = findTypeDecl(typ)
    val typeNodeId = addApiTypeNode(td)

    def addBodyDecl(bd : AST.BodyDecl) : Unit = {
      if(bd == null)
        System.err.println("Method or constructor" + bodydeclName + " not found in the program ...")
      else
        bd.buildDG(this, typeNodeId)
    }

    nodeKind match {
      case "type" => ()
      case "method" => addBodyDecl (td findMethodBySig bodydeclName)
      case "constructor" => addBodyDecl (td findConstructorBySignature ("#_" + bodydeclName))

      case _ => System.err.println("node kind unknown")
    }
  }



  def addStringLiteral(literal: String, occurrences: _root_.java.util.Collection[AST.BodyDecl]) : Unit = {

    def stringType = {
      val td = findTypeDecl("java.lang.string")
      val nid = addApiTypeNode(td)
      NamedType(nid)
    }

    println("string "+literal + " "+ occurrences.size()+" occurences" )

    for(bd <- occurrences){
      val packageNode = nodesByName(bd.hostBodyDecl.compilationUnit.getPackageDecl)

      val bdNode = bd buildDGNode this
      val strNode = addNode(bd.fullName()+literal, literal, Literal, mutable = false)
      /*
        this is obviously wrong: TODO FIX
      */
      addContains(packageNode, strNode)
      setType(strNode, stringType)
      addEdge(Uses(bdNode, strNode, Some(Read)))
    }
  }

  def addApiTypeNode(td: AST.TypeDecl): NodeIdT = {
    val packageNode = addPackage(td.packageName(), mutable = false)
    val tdNode = addNode(td.fullName(), td.name(), td.getDGNodeKind, mutable = false)
    addContains(packageNode, tdNode)
    tdNode
  }

  def addApiTypeNodeAndRegister(td: AST.TypeDecl): Unit =
    registerDecl(addApiTypeNode(td), td)


  def addBodyDecl(bd : AST.BodyDecl) : Unit = {
    val typeNodeId = addApiTypeNode(bd.hostType())
    bd.buildDG(this, typeNodeId)
  }

  private def throwRegisteringError(n : ConcreteNode, astType : String) =
    throw new Error(s"Wrong registering ! AGNode.kind : ${n.kind} while AST.Node is an $astType")



  def register( nid : NodeIdT,
                kindExpected : JavaNodeKind,
                declHolder : => ASTNodeLink,
                kindFound : String ): Unit ={
    if(g.getConcreteNode(nid).kind == kindExpected)
      graph2ASTMap += (nid -> declHolder)
    //g = g.setInternal(nid, declHolder)
    else
      throwRegisteringError(g.getConcreteNode(nid), kindFound)
  }

  def registerDecl(n : NodeIdT, decl : AST.InterfaceDecl) =
    register(n, Interface, InterfaceDeclHolder(decl), "InterfaceDecl")

  def registerDecl(n : NodeIdT, decl : AST.ClassDecl) =
    register(n, Class, ClassDeclHolder(decl), "ClassDecl")

  def registerDecl(n : NodeIdT, decl : AST.TypeVariable) =
    register(n, TypeVariable, TypeVariableHolder(decl), "TypeVariable")

  def registerDecl(n : NodeIdT, decl : AST.WildcardType) =
    register(n, WildCardType, WildCardTypeHolder(decl), "WildCardType")

  def registerDecl(n : NodeIdT, decl : AST.TypeDecl) =
    register(n, Primitive, PrimitiveDeclHolder(decl), "PrimitiveType")

  /*def registerDecl(n : NodeIdT, decl : AST.PrimitiveType){
    g.getNode(n).kind match {
      case Primitive =>
        g = g.setInternal(n, PrimitiveDeclHolder(Some(decl)))
      case _ => throwRegisteringError(g.getNode(n), "PrimitiveType")
    }
  }*/

  def registerDecl(n : NodeIdT, decl : AST.ConstructorDecl)=
    register(n, Constructor, ConstructorDeclHolder(decl), "ConstructorDecl")


  def registerDecl(n : NodeIdT, decl : AST.MethodDecl) : Unit = {
    g.getConcreteNode(n).kind match {
      case Method | StaticMethod | AbstractMethod =>
        graph2ASTMap += (n -> MethodDeclHolder(decl))
      case _ => throwRegisteringError(g.getConcreteNode(n), "MethodDecl")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.FieldDeclaration) : Unit = {
    g.getConcreteNode(n).kind match {
      case Field | StaticField=>
        graph2ASTMap += (n -> FieldDeclHolder(decl))
      case _ => throwRegisteringError(g.getConcreteNode(n), "FieldDeclaration")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.ParameterDeclaration) : Unit =
    register(n, Param, ParameterDeclHolder(decl), "ParameterDeclaration")

  def registerDef(n : NodeIdT, decl : AST.Expr) : Unit =
    register(n, Definition, ExprHolder(decl), "Expr")

  def registerDef(n : NodeIdT, decl : AST.Block) : Unit =
    register(n, Definition, BlockHolder(decl), "Block")
}
