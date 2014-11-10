package puck.javaAG.immutable

import puck.graph.immutable.constraints.ConstraintsMaps
import puck.graph.immutable.{NoType, AGNode, GraphBuilder}
import puck.graph.immutable.transformations.Recording
import puck.javaAG._
import puck.javaAG.immutable.nodeKind._
import puck.util.PuckNoopLogger
import scala.collection.JavaConversions.collectionAsScalaIterable

import puck.graph.immutable.AccessGraph._
/**
 * Created by lorilan on 29/10/14.
 */
class JavaGraphBuilder(program : AST.Program) extends GraphBuilder(JavaNode){
  var idSeed = rootId + 1

    val root = (rootId, rootName, JavaRoot, NoType, true, EmptyDeclHolder)

   g = new JavaAccessGraph(program, PuckNoopLogger, {() => val id = idSeed; idSeed += 1; id},
   NodeIndex() + (rootId -> root), NodeIndex(),
    EdgeMap(), EdgeMap(), EdgeMap(),
    Node2NodeMap(), EdgeMap(), EdgeMap(),
    UseDependencyMap(), UseDependencyMap(),
    AbstractionMap(), ConstraintsMaps(), Recording())

  def addPredefined( p : Predefined): Unit = {
    super.addPredefined(p.id, p.fullName, p.name, p.kind, EmptyDeclHolder)
  }

  Predefined.list foreach addPredefined

  def addPackageNode(fullName: String, localName:String) : NodeIdT =
    super.addNode(fullName, localName, Package, NoType)

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
    val tdNode = addNode(td.fullName(), td.name(), td.getAGNodeKind, NoType)
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
      val strNode = addNode(bd.fullName()+literal, literal, Literal, Predefined.stringTyp)
      //TODO set type of node to string

      /*
        this is obviously wrong: TODO FIX
      */
      addContains(packageNode, strNode)
      addUses(bdNode, strNode)
    }
  }

  private def throwRegisteringError(n : AGNode, astType : String) =
    throw new Error("Wrong registering ! AGNode.kind : %s while AST.Node is an %s".format(n.kind, astType))


  def registerDecl(n : NodeIdT, decl : AST.InterfaceDecl){
    g.getNode(n).kind match {
      case Interface =>
        g = g.setInternal(n, InterfaceDeclHolder(Some(decl)))
      case _ => throwRegisteringError(g.getNode(n), "InterfaceDecl")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.ClassDecl){
    g.getNode(n).kind match {
      case Class =>
        g = g.setInternal(n, ClassDeclHolder(Some(decl)))
      case _ => throwRegisteringError(g.getNode(n), "ClassDecl")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.ConstructorDecl){
    g.getNode(n).kind match {
      case Constructor =>
        g = g.setInternal(n, ConstructorDeclHolder(Some(decl)))
      case _ => throwRegisteringError(g.getNode(n), "ConstructorDecl")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.MethodDecl){
    g.getNode(n).kind match {
      case Method =>
        g = g.setInternal(n, ConcreteMethodDeclHolder(Some(decl)))
      case AbstractMethod =>
        g = g.setInternal(n, AbstractMethodDeclHolder(Some(decl)))
      case _ => throwRegisteringError(g.getNode(n), "MethodDecl")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.FieldDeclaration){
    g.getNode(n).kind match {
      case Field =>
        g = g.setInternal(n, FieldDeclHolder(Some(decl)))
      case _ => throwRegisteringError(g.getNode(n), "FieldDeclaration")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.PrimitiveType){
    g.getNode(n).kind match {
      case Primitive =>
        g = g.setInternal(n, PrimitiveDeclHolder(Some(decl)))
      case _ => throwRegisteringError(g.getNode(n), "PrimitiveType")
    }
  }

}
