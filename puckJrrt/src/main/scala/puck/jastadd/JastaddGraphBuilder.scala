package puck.jastadd

import org.extendj.ast
import org.extendj.ast.{List => _, Set => _, _} //import everything except List and Set
import puck.graph._
import puck.javaGraph.{nodeKind, JavaGraphBuilder}
import puck.javaGraph.nodeKind._

import scala.collection.JavaConversions.collectionAsScalaIterable

object JastaddGraphBuilder {

  //JavaAccessor

  def isa(n1 : NodeId, n2 : NodeId) =
      new DGEdge(Isa, n1, n2)

  def classKind : JavaNodeKind = Class
  //def innerClassKind : JavaNodeKind = InnerClass

  def interfaceKind : JavaNodeKind = Interface
  //def innerInterfaceKind : JavaNodeKind= InnerInterface

  def genInterface(gid: GenericInterfaceDecl) : JavaNodeKind = GenericInterface
  def genClass(gid: GenericClassDecl) : JavaNodeKind = GenericClass

  /*{
    Range.inclusive(gid.getTypeParameters.getNumChild - 1, 0, -1 ).foldLeft(List[Variance]()){
      case (l, i) =>
        gid.getTypeParameter(i)
    }

  }*/

  def field = Field
  def staticField = StaticField
  def constructor = Constructor
  def abstractMethod = AbstractMethod
  def method = Method
  def staticMethod = StaticMethod

  def parameter = Param

  def definition = Definition

  def primitive = Primitive
  def typeVariable = nodeKind.TypeVariable
  def wildcardType = WildCardType

  def definitionName = DependencyGraph.definitionName
}

class JastaddGraphBuilder(val program : Program) extends JavaGraphBuilder {
  var graph2ASTMap = Map[Int, ASTNodeLink]()

  def findTypeDecl(typ : String): TypeDecl ={
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
            case Some(ConstructorDeclHolder(cdecl)) =>
              nodesByName get cdecl.hostType().fullName() match {
                case Some(pid) => cdecl.buildDG(this, pid)
                case _ => addBodyDecl(cdecl)
              }
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

    def addBodyDecl(bd : BodyDecl) : Unit = {
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



  def addStringLiteral(literal: String, occurrences: java.util.Collection[BodyDecl]) : Unit = {

    def stringType = {
      val td = findTypeDecl("java.lang.string")
      val nid = addApiTypeNode(td)
      NamedType(nid)
    }

    println("string "+literal + " "+ occurrences.size()+" occurences" )

    for(bd <- occurrences){
      val packageNode = nodesByName(bd.hostBodyDecl.compilationUnit.getPackageDecl)

      val bdNode = bd buildDGNode this
      val strNode = addNode(bd.fullName()+literal, literal, nodeKind.Literal, mutable = false)
      /*
        this is obviously wrong: TODO FIX
      */
      addContains(packageNode, strNode)
      setType(strNode, stringType)
      addEdge(Uses(bdNode, strNode, Some(Read)))
    }
  }

  def addApiTypeNode(td: TypeDecl): NodeIdT = {

    val tdNode = addNode(td.fullName(), td.name(), td.getDGNodeKind, mutable = false)
//
//    def aux(n : ASTNode[T] forSome {type T <: ASTNode[_]}) :  NodeId = n match {
//      case td2 : TypeDecl =>
//        nodesByName get td2.fullName() match {
//          case None => addApiTypeNode(td2)
//          case Some(td2n) => td2n
//        }
//      case cu : CompilationUnit =>
//        addPackage(cu.getPackageDecl, mutable = false)
//      case _ => aux(n.getParent.asInstanceOf[ASTNode[ASTNode[_]]])
//    }
//    val cterId = aux(td.getParent.asInstanceOf[ASTNode[ASTNode[_]]])
    val cterId = addPackage(td.packageName(), mutable = false)
    addContains(cterId, tdNode)
    tdNode
  }

  def addApiTypeNodeAndRegister(td: TypeDecl): Unit =
    registerDecl(addApiTypeNode(td), td)


  def addBodyDecl(bd : BodyDecl) : Unit = {
    val typeNodeId = addApiTypeNode(bd.hostType())
    bd.buildDG(this, typeNodeId)
  }

  private def throwRegisteringError(n : ConcreteNode, astType : String) =
    throw new Error(s"Wrong registering ! AGNode.kind : ${n.kind} while Node is an $astType")

  def getNode(namedElement: DGNamedElement) : Int =
    try getNodeByName(namedElement.fullName)
    catch {
      case e: NoSuchElementException =>
        val node: Int =
          addNode(namedElement.fullName,
            namedElement.name,
            namedElement.getDGNodeKind,
            namedElement.fromSource)
        namedElement.registerNode(this, node)
        node

    }


  def getType(a : Access) : Type = {
    a.lock()
    a match {
      case aa: ArrayTypeAccess =>
        ParameterizedType(arrayTypeId, List(getType(aa.getAccess)))

      case ta: TypeAccess =>
        NamedType(getNode(ta.decl()))
      case d: Dot =>
        if (d.isRightRotated)
          d.rotateLeft()
        getType(d.getRight)

      case pta: ParTypeAccess =>
        val genId = getNode(pta.genericDecl())
        val args: List[Type] =
          Range.inclusive(pta.getNumTypeArgument - 1, 0, -1).foldLeft(List[Type]()) {
            case (l, i) =>
              getType(pta.getTypeArgument(i)) :: l
          }
        ParameterizedType(genId, args)

      case we: WildcardExtends =>
        Covariant(getType(we.getAccess))
      case ws: WildcardSuper =>
        Covariant(getType(ws.getAccess))
      case w: Wildcard =>
        NamedType(getNode(w.`type`()))
      case _ => throw new Error(s"getType, ${a.compilationUnit().pathName()} line ${a.location()} " +
        s"access.getClass == ${a.getClass}")
    }
  }

//  ta.decl() match {
//    case ptd : ParTypeDecl =>
//      val genId = getNode(ptd.genericDecl())
//      val args : List[Type] =
//        Range.inclusive(ptd.getNumArgument-1, 0, -1).foldLeft(List[Type]()){
//          case (l, i) =>
//            getType(ptd.getArgument(i).asInstanceOf[TypeAccess]) :: l
//        }
//      ParameterizedType(genId, args)
//    case td : TypeDecl => NamedType(getNode(td))
//    case k =>  throw new Error(s"getType, t.decl.getClass == ${k.getClass}")
//  }



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

  def registerDecl(n : NodeIdT, decl : InterfaceDecl) =
    register(n, Interface, InterfaceDeclHolder(decl), "InterfaceDecl")

  def registerDecl(n : NodeIdT, decl : ClassDecl) =
    register(n, Class, ClassDeclHolder(decl), "ClassDecl")

  def registerDecl(n : NodeIdT, decl : GenericInterfaceDecl) =
    register(n, GenericInterface, InterfaceDeclHolder(decl), "GenericInterfaceDecl")

  def registerDecl(n : NodeIdT, decl : GenericClassDecl) =
    register(n, GenericClass, ClassDeclHolder(decl), "GenericClassDecl")

  def registerDecl(n : NodeIdT, decl : ast.TypeVariable) =
    register(n, nodeKind.TypeVariable, TypeVariableHolder(decl), "TypeVariable")

  def registerDecl(n : NodeIdT, decl : WildcardType) =
    register(n, WildCardType, WildCardTypeHolder(decl), "WildCardType")

  def registerDecl(n : NodeIdT, decl : TypeDecl) =
    register(n, Primitive, PrimitiveDeclHolder(decl), "PrimitiveType")

  def registerDecl(n : NodeIdT, decl : ConstructorDecl)=
    register(n, Constructor, ConstructorDeclHolder(decl), "ConstructorDecl")

  def registerDecl(n : NodeIdT, decl : MethodDecl) : Unit =
    register(n, Set[NodeKind](Method, StaticMethod, AbstractMethod), MethodDeclHolder(decl) , "MethodDecl")

  def registerDecl(n : NodeIdT, decl : FieldDeclaration) : Unit =
    register(n, Set[NodeKind](Field, StaticField), FieldDeclHolder(decl), "FieldDeclaration")

  def registerDecl(n : NodeIdT, decl : ParameterDeclaration) : Unit =
    register(n, Param, ParameterDeclHolder(decl), "ParameterDeclaration")

  def registerDef(n : NodeIdT, decl : Expr) : Unit =
    register(n, Definition, ExprHolder(decl), "Expr")

  def registerDef(n : NodeIdT, decl : Block) : Unit =
    register(n, Definition, BlockHolder(decl), "Block")


}
