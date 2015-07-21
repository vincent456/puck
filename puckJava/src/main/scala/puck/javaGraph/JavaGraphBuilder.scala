package puck.javaGraph

import puck.graph._
import puck.graph.constraints.{SupertypeAbstraction, ConstraintsMaps}
import puck.javaGraph.nodeKind._
import scala.collection.JavaConversions.collectionAsScalaIterable

import DependencyGraph._

class JavaGraphBuilder(val program : AST.Program) extends GraphBuilder{

   val root = ConcreteNode(rootId, rootName, Package, None, mutable = true)

   g = new DependencyGraph(JavaNodeKind,
     NodeIndex(root), EdgeMap(),
     AbstractionMap(), ConstraintsMaps(), Recording())

  var graph2ASTMap = Map[Int, ASTNodeLink]()

  def addDefinitionNode() : NodeIdT = addAnonymousNode(Definition)

  def addPackageNode(fullName: String, localName:String) : NodeIdT =
    super.addNode(fullName, localName, Package, None)

  def getDefinition(nid : NodeId) =
    g.getConcreteNode(nid).definition_!(g)


  def addPackage(p : String, mutable : Boolean): NodeIdT =
    nodesByName get p match {
      case None =>
        val fp = filterPackageName(p)
        val path = fp split "[.]"
        if (path.isEmpty)
          addPackageNode(fp, fp)
        else {
          val (_, n):(StringBuilder, NodeIdT) = path.foldLeft(new StringBuilder(), rootId){
            (sb_nodeParent:(StringBuilder, NodeIdT), p:String) => sb_nodeParent match {
              case (sb, nodeParent) =>
                sb append p
                val nId = addPackageNode(sb.toString(), p)
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
      throw new DGBuildingError(typ + " not found")
    td
  }

  def addApiTypeNode(td: AST.TypeDecl): NodeIdT = {
    //println("adding api td " + td.fullName() + " with packagedecl " + td.packageName())
    val packageNode = addPackage(td.packageName(), mutable = false)
    val tdNode = addNode(td.fullName(), td.name(), td.getDGNodeKind, None)
    setMutability(tdNode, mutable = false)

    /*if(doAddUses)
      for (use <- td.uses()) {
        addUses(use.buildAGNode(this), tdNode)
      }
  */
    addContains(packageNode, tdNode)

    tdNode
  }

  def addBodyDecl(bd : AST.BodyDecl) : Unit = {
    val typeNodeId = addApiTypeNode(bd.hostType())
    bd.buildDG(this, typeNodeId)
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

  def addApiNode(nodeKind : String, typ : String, bodydeclName: String) : Unit = {
    println("trying to add type "+typ + " " + bodydeclName+ " ... ")

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
      Some(new JavaNamedType(nid))
    }

    println("string "+literal + " "+ occurrences.size()+" occurences" )

    for(bd <- occurrences){
      val packageNode = nodesByName(bd.hostBodyDecl.compilationUnit.getPackageDecl)

      val bdNode = bd buildDGNode this
      val strNode = addNode(bd.fullName()+literal, literal, Literal, stringType)
      /*
        this is obviously wrong: TODO FIX
      */
      addContains(packageNode, strNode)
      addEdge(Uses(bdNode, strNode, Some(Read)))
    }
  }

  private def throwRegisteringError(n : ConcreteNode, astType : String) =
    throw new Error(s"Wrong registering ! AGNode.kind : ${n.kind} while AST.Node is an $astType")



  def register(
        nid : NodeIdT,
        kindExpected : JavaNodeKind,
        declHolder : => ASTNodeLink,
        kindFound : String): Unit ={
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
      case Method | StaticMethod=>
        graph2ASTMap += (n -> ConcreteMethodDeclHolder(decl))
      case AbstractMethod =>
        graph2ASTMap += (n -> AbstractMethodDeclHolder(decl))
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

  def registerDef(n : NodeIdT, decl : AST.Expr) : Unit = {
    g.getConcreteNode(n).kind match {
      case Definition =>
        graph2ASTMap += (n -> ExprHolder(decl))
      case _ => throwRegisteringError(g.getConcreteNode(n), "Expr")
    }
  }

  def registerDef(n : NodeIdT, decl : AST.Block) : Unit = {
    g.getConcreteNode(n).kind match {
      case Definition =>
        graph2ASTMap += (n -> BlockHolder(decl))
      case _ => throwRegisteringError(g.getConcreteNode(n), "Expr")
    }
  }


  override def registerAbstraction : DependencyGraph => (ImplId, Abstraction) => DependencyGraph =
    graph => (implId , abs) =>
      abs match {
        case AccessAbstraction(absId, SupertypeAbstraction) =>
          val impl = graph.getConcreteNode(implId)
          val absNode = graph.getConcreteNode(absId)
          (impl.kind, absNode.kind) match {
            /*case (Class, Class)
              | (Class, Interface)
              | (Interface, Interface) =>
            */
            case (Class, Interface) =>
              val absMeths = graph.content(absId).map(graph.getConcreteNode)
              val candidates = graph.content(impl.id).map(graph.getConcreteNode)
              Type.findAndRegisterOverridedInList(graph, absMeths.toList, candidates.toList) {
                Type.errorOnImplemNotFound(graph.fullName(impl.id))
              } .value.getOrElse(sys.error("Success expected"))
                .addAbstraction(implId, abs)
            case _ => graph
          }
        case _ => super.registerAbstraction(graph)(implId , abs)
      }

}
