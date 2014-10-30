package puck.javaAG.immutable

import puck.graph.immutable.GraphBuilder
import puck.graph.immutable.transformations.Recording
import puck.javaAG._
import puck.javaAG.immutable.nodeKind._
import puck.util.PuckNoopLogger
import scala.collection.JavaConversions.collectionAsScalaIterable

import puck.graph.immutable.AccessGraph._
/**
 * Created by lorilan on 29/10/14.
 */
class JavaGraphBuilder(program : AST.Program) extends GraphBuilder[JavaNodeKind](JavaNode){
   g = new JavaAccessGraph(program, PuckNoopLogger, 1,
    NodeSet(), EdgeMap(), EdgeMap(), EdgeMap(),
    Node2NodeMap() + (0 -> 0), EdgeMap(), EdgeMap(),
    UseDependencyMap(), UseDependencyMap(),
    AbstractionMap(), Recording())


  def addPackageNode(fullName: String, localName:String) : NodeIdT =
    super.addNode(fullName, localName, JavaNodeKind.packageKind)

  def addPackage(p : String, mutable : Boolean): NodeIdT =
    nodesByName get p match {
      case None =>
        val fp = filterPackageName(p)
        val path = fp split "[.]"
        if (path.length == 0)
          addPackageNode(fp, fp)
        else {
          val (_, n):(StringBuilder, NodeIdT) = path.foldLeft(new StringBuilder(), rootId){
            (sb_nodeParent:(StringBuilder, NodeIdT), p:String) => sb_nodeParent match {
              case (sb, nodeParent) =>
                sb append p
                val nId = addPackageNode(sb.toString, p)
                addContains(nodeParent, nId)
                setMutability(nId, mutable)
                sb append "."
                (sb, nId)
            }
          }
          n
        }
      case  Some(pn) => pn
    }



  def addApiTypeNode(td: AST.TypeDecl, doAddUses : Boolean): NodeIdT = {
    //println("adding api td " + td.fullName())
    val packageNode = addPackage(td.compilationUnit().getPackageDecl, mutable = false)
    val tdNode = addNode(td.fullName(), td.name(), td.getAGNodeKind)
    setMutability(tdNode, mutable = false)

    if(doAddUses)
      for (use <- td.uses()) {
        addUses(use.buildAGNode(this), tdNode)
      }

    addContains(packageNode, tdNode)

    tdNode
  }


  def addApiNode(program : AST.Program, 
                 nodeKind : String, 
                 typ : String, bodydeclName: String){
    println("trying to add type "+typ + " " + bodydeclName+ " ... ")
    val td = program findType typ
    if(td == null){
      System.err.println(typ + " not found")
      return
    }

    val tdNode = addApiTypeNode(td, doAddUses = true)

    def addBodyDecl(bd : AST.BodyDecl){
      if(bd == null)
        System.err.println("Method or constructor" + bodydeclName + " not found in the program ...")
      else
        addContains(tdNode, bd buildAG this)
    }

    nodeKind match {
      case "type" => ()
      case "method" => addBodyDecl (td findMethodBySig bodydeclName)
      case "constructor" => addBodyDecl (td findConstructorBySignature ("#_" + bodydeclName))

      case _ => System.err.println("node kind unknown")
    }
  }



  def addStringLiteral(literal: String, occurrences: _root_.java.util.Collection[AST.BodyDecl]){
    println("string "+literal + " "+ occurrences.size()+" occurences" )

    for(bd <- occurrences){
      val packageNode = nodesByName(bd.hostBodyDecl.compilationUnit.getPackageDecl)

      val bdNode = bd buildAGNode this
      val strNode = addNode(bd.fullName()+literal, literal, Predefined.stringLiteralPrototype)

      /*
        this is obviously wrong: TODO FIX
      */
      addContains(packageNode, strNode)
      addUses(bdNode, strNode)
    }
  }

  private def throwRegisteringError(n : NodeIdT, astType : String) =
    throw new Error("Wrong registering ! AGNode.kind : %s while AST.Node is an %s".format(n, astType))


  def registerDecl(n : NodeIdT, decl : AST.InterfaceDecl){
    g.getNode(n).kind match {
      case Interface(_,_) =>
        g = g.setKind(n, Interface(n, Some(decl))).graph
      case _ => throwRegisteringError(n, "InterfaceDecl")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.ClassDecl){
    g.getNode(n).kind match {
      case Class(_, _) =>
        g = g.setKind(n, Class(n, Some(decl))).graph
      case _ => throwRegisteringError(n, "ClassDecl")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.ConstructorDecl){
    g.getNode(n).kind match {
      case Constructor(_,typ,_) =>
        g = g.setKind(n, Constructor(n, typ , Some(decl))).graph
      case _ => throwRegisteringError(n, "ConstructorDecl")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.MethodDecl){
    g.getNode(n).kind match {
      case Method(_, typ, _) =>
        g = g.setKind(n, Method(n, typ , Some(decl))).graph
      case AbstractMethod(_, typ, _) =>
        g = g.setKind(n, AbstractMethod(n, typ , Some(decl))).graph
      case _ => throwRegisteringError(n, "MethodDecl")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.FieldDeclaration){
    g.getNode(n).kind match {
      case Field(_, typ, _) =>
        g = g.setKind(n, Field(n, typ, Some(decl))).graph
      case _ => throwRegisteringError(n, "FieldDeclaration")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.PrimitiveType){
    g.getNode(n).kind match {
      case Primitive(_, _) =>
        g = g.setKind(n, Primitive(n, Some(decl))).graph
      case _ => throwRegisteringError(n, "PrimitiveType")
    }
  }

}
