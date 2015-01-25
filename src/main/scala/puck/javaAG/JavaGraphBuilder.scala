package puck.javaAG

import puck.graph._
import puck.graph.AccessGraph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.Recording
import puck.javaAG.nodeKind._
import puck.util.PuckNoopLogger
import scala.collection.JavaConversions.collectionAsScalaIterable

/**
 * Created by lorilan on 29/10/14.
 */
class JavaGraphBuilder(val program : AST.Program) extends GraphBuilder(JavaNode){
  var idSeed = rootId + 1

    val root = (rootId, rootName, JavaRoot, NoType, true, EmptyDeclHolder)

   g = new JavaAccessGraph(program, PuckNoopLogger, {() => val id = idSeed; idSeed += 1; id},
   NodeIndex() + (rootId -> root), NodeIndex(),
    EdgeMap(), EdgeMap(), EdgeMap(),
    Node2NodeMap(), EdgeMap(), EdgeMap(),
    UseDependencyMap(), UseDependencyMap(),
    AbstractionMap(), ConstraintsMaps(), Recording())

  /*def addPredefined( p : Predefined): Unit = {
    super.addPredefined(p.id, p.fullName, p.name, p.kind, EmptyDeclHolder)
  }

  Predefined.list foreach addPredefined*/

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



  def findTypeDecl(typ : String): AST.TypeDecl ={
    val td = program findType typ
    if(td == null)
      throw new AGBuildingError(typ + " not found")
    td
  }

  def addApiTypeNode(td: AST.TypeDecl): NodeIdT = {
    //println("adding api td " + td.fullName() + " with packagedecl " + td.packageName())
    val packageNode = addPackage(td.packageName(), mutable = false)
    val tdNode = addNode(td.fullName(), td.name(), td.getAGNodeKind, NoType)
    setMutability(tdNode, mutable = false)

    /*if(doAddUses)
      for (use <- td.uses()) {
        addUses(use.buildAGNode(this), tdNode)
      }
  */
    addContains(packageNode, tdNode)

    tdNode
  }

  def addBodyDecl(bd : AST.BodyDecl){
    val typeNodeId = addApiTypeNode(bd.hostType())
    bd.buildAG(this, typeNodeId)
  }

  def attachOrphanNodes(fromId : Int = g.rootId){
    val lastId = g.numNodes - 1
    if(fromId < lastId){
      for(nodeId <- fromId to lastId){
        //println(s"${g.container(nodeId)} contains $nodeId")
        if(g.container(nodeId).isEmpty && nodeId != g.rootId){
          val n = g.getNode(nodeId).asInstanceOf[JavaNode]
          //println(s"orphan node : ${n.fullName}  : ${n.kind} - container = ${n.container}")
          n.t match {
            case FieldDeclHolder(Some(d)) => addBodyDecl(d)
            case mdh : MethodDeclHolder => mdh.decl.foreach(addBodyDecl)
            case ConstructorDeclHolder(Some(cdecl)) => addBodyDecl(cdecl)
            case tdh : TypedKindDeclHolder => tdh.decl.foreach(addApiTypeNode)
            case _ =>
              println( n.fullName + " " + n.t + " attach orphan nodes unhandled case")
              ()
          }
        }
      }
      //addBodyDecl (case MethodDeclHolder) can add new typeNodes
      attachOrphanNodes(lastId)
    }
  }

  def addApiNode(nodeKind : String, typ : String, bodydeclName: String){
    println("trying to add type "+typ + " " + bodydeclName+ " ... ")

    val td = findTypeDecl(typ)
    val typeNodeId = addApiTypeNode(td)

    def addBodyDecl(bd : AST.BodyDecl){
      if(bd == null)
        System.err.println("Method or constructor" + bodydeclName + " not found in the program ...")
      else
        bd.buildAG(this, typeNodeId)
    }

    nodeKind match {
      case "type" => ()
      case "method" => addBodyDecl (td findMethodBySig bodydeclName)
      case "constructor" => addBodyDecl (td findConstructorBySignature ("#_" + bodydeclName))

      case _ => System.err.println("node kind unknown")
    }
  }



  def addStringLiteral(literal: String, occurrences: _root_.java.util.Collection[AST.BodyDecl]){

    def stringType = {
      val td = findTypeDecl("java.lang.string")
      val nid = addApiTypeNode(td)
      NamedTypeHolder(new JavaNamedType(nid, "string"))
    }



    println("string "+literal + " "+ occurrences.size()+" occurences" )

    for(bd <- occurrences){
      val packageNode = nodesByName(bd.hostBodyDecl.compilationUnit.getPackageDecl)

      val bdNode = bd buildAGNode this
      val strNode = addNode(bd.fullName()+literal, literal, Literal, stringType)
      //TODO set type of node to string

      /*
        this is obviously wrong: TODO FIX
      */
      addContains(packageNode, strNode)
      addUses(bdNode, strNode)
    }
  }

  private def throwRegisteringError(n : AGNode, astType : String) =
    throw new Error(s"Wrong registering ! AGNode.kind : ${n.kind} while AST.Node is an $astType")

  def register(
        nid : NodeIdT,
        kindExpected : JavaNodeKind,
        declHolder : => DeclHolder,
        kindFound : String): Unit ={
    if(g.getNode(nid).kind == kindExpected)
      g = g.setInternal(nid, declHolder)
    else
      throwRegisteringError(g.getNode(nid), kindFound)
  }

  def registerDecl(n : NodeIdT, decl : AST.InterfaceDecl) =
    register(n, Interface, InterfaceDeclHolder(Some(decl)), "InterfaceDecl")

  def registerDecl(n : NodeIdT, decl : AST.ClassDecl) =
    register(n, Class, ClassDeclHolder(Some(decl)), "ClassDecl")

  def registerDecl(n : NodeIdT, decl : AST.TypeVariable) =
    register(n, TypeVariable, TypeVariableHolder(Some(decl)), "TypeVariable")

  def registerDecl(n : NodeIdT, decl : AST.TypeDecl) =
    register(n, Primitive, PrimitiveDeclHolder(Some(decl)), "PrimitiveType")

  /*def registerDecl(n : NodeIdT, decl : AST.PrimitiveType){
    g.getNode(n).kind match {
      case Primitive =>
        g = g.setInternal(n, PrimitiveDeclHolder(Some(decl)))
      case _ => throwRegisteringError(g.getNode(n), "PrimitiveType")
    }
  }*/

  def registerDecl(n : NodeIdT, decl : AST.ConstructorDecl)=
    register(n, Constructor, ConstructorDeclHolder(Some(decl)), "ConstructorDecl")


  def registerDecl(n : NodeIdT, decl : AST.MethodDecl){
    g.getNode(n).kind match {
      case Method =>
        g = g.setInternal(n, ConcreteMethodDeclHolder(Some(decl)))
      case AbstractMethod =>
        g = g.setInternal(n, AbstractMethodDeclHolder(Some(decl)))
      case _ => throwRegisteringError(g.getNode(n), "MethodDecl")
    }
  }

  def registerDecl(n : NodeIdT, decl : AST.FieldDeclaration) =
    register(n, Field, FieldDeclHolder(Some(decl)), "FieldDeclaration")

}
