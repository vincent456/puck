package puck.javaAG.mutable

import puck.graph.mutable.AGNode
import puck.javaAG.mutable.nodeKind._

import scala.collection.mutable.StringBuilder
import scala.collection.{mutable => smutable}
import scala.collection.JavaConversions.collectionAsScalaIterable
/**
 * Created by lorilan on 30/10/14.
 */
class JavaGraphBuilder(prog : AST.Program) {

  val jgraph = new JavaAccessGraph()
  Predefined.list.foreach{ p =>
    addNode(p.fullName, p.name, p.kind)
  }

  jgraph.root.kind match {
    case r @ JavaRoot() => r.program = prog

      Predefined.list.foreach{
        case p @ Predefined(pkg, name, _) =>

          val decl = prog.findType(pkg, name)

          if(decl == null){
            throw new Error(name + " : " + nodesByName(p.fullName).kind + ", decl not found")
          }
          (nodesByName(p.fullName).kind, decl) match {
            case (p @ Primitive(), d : AST.TypeDecl) => p.decl = d
            case (c @ Class(), d : AST.ClassDecl) => c.decl = d
            case (k, d) => println( k + " with decl of type " +d.getClass + " : case unhandled")
          }
      }
    case _ => assert(false)
  }


  private [this] val nodesByName = smutable.Map[String, AGNode[JavaNodeKind]]()
  def getNodeByName( k : String) : Int = nodesByName(k).id //java accessor
  private [this] val nodeById = smutable.Map[Int, AGNode[JavaNodeKind]]()

  def addNode(unambiguousFullName: String, localName:String, kind: JavaNodeKind): Int = {
    nodesByName get unambiguousFullName match {
      case None =>
        val n = jgraph.addNode(localName, kind)
        nodesByName += (unambiguousFullName -> n)
        n.id
      case Some(n) => n.id /* check that the kind and type is indeed the same ??*/
    }
  }

  def addPackageNode(fullName: String, localName:String) : Int =
    addNode(fullName, localName, puck.javaAG.mutable.nodeKind.JavaNodeKind.`package`)

  def addPackage(p : String, mutable : Boolean): Int =
    nodesByName get (p) match {
      case None =>
        val fp = JavaAccessGraph.filterPackageName(p)
        val path = fp split "[.]"
        if (path.length == 0)
          addPackageNode(fp, fp)
        else {
          val (_, n):(StringBuilder, AGNode[JavaNodeKind]) = path.foldLeft(new StringBuilder(), root){
            (sb_nodeParent:(StringBuilder, AGNode[JavaNodeKind]), p:String) => sb_nodeParent match {
              case (sb, nodeParent) =>
                sb append p
                val nid = addPackageNode(sb.toString, p)
                val n = nodeById(nid)
                n.isMutable = mutable
                nodeParent.content += n
                sb append "."
                (sb, n)
            }
          }
          n.id
        }
      case  Some(pn) => pn.id
    }



  def addApiTypeNode(td: AST.TypeDecl, addUses : Boolean): Int = {
    //println("adding api td " + td.fullName())
    val packageNode = addPackage(td.compilationUnit().getPackageDecl, mutable = false)
    val tdNodeId = addNode(td.fullName(), td.name(), td.getAGNodeKind)
    val tdNode = nodeById(tdNodeId)
    tdNode.isMutable = false

    if(addUses)
      for (use <- td.uses()) {
        tdNode.users += nodeById(use.buildAGNode(this))
      }

    //    try{
    nodeById(packageNode).content += tdNode
    //    }catch{
    //      case e : IllegalAGOperation => ()
    //    }

    tdNodeId
  }


  def addApiNode(program : AST.Program, nodeKind : String, `type` : String, bodydeclName: String){
    println("trying to add type "+`type` + " " + bodydeclName+ " ... ")
    val td = program  findType `type`
    if(td == null){
      System.err.println(`type` + " not found")
      return
    }

    val tdNode = addApiTypeNode(td, addUses = true)

    def addBodyDecl(bd : AST.BodyDecl){
      if(bd == null)
        System.err.println("Method or constructor" + bodydeclName + " not found in the program ...")
      else
        nodeById(tdNode).content += nodeById(bd buildAG this)
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
      val strNode = addNode(bd.fullName()+literal, literal,
        puck.javaAG.mutable.nodeKind.JavaNodeKind.literal(new JavaNamedType(nodesByName("java.lang.String"))))

      /*
        this is obviously wrong: TODO FIX
      */
      packageNode.content += nodeById(strNode)
      nodeById(strNode).users += nodeById(bdNode)
    }
  }

  private def throwRegisteringError(n : AGNode[JavaNodeKind], astType : String) =
    throw new Error("Wrong registering ! AGNode.kind : %s while AST.Node is an %s".format(n.kind, astType))


  def registerDecl(n : AGNode[JavaNodeKind], decl : AST.InterfaceDecl){
    n.kind match {
      case i @ Interface() =>
        i.decl = decl
      case _ => throwRegisteringError(n, "InterfaceDecl")
    }
  }

  def registerDecl(n : AGNode[JavaNodeKind], decl : AST.ClassDecl){
    n.kind match {
      case c @ Class() => c.decl = decl
      case _ => throwRegisteringError(n, "ClassDecl")
    }
  }

  def registerDecl(n : AGNode[JavaNodeKind], decl : AST.ConstructorDecl){
    n.kind match {
      case c @ Constructor() => c.decl = decl
      case _ => throwRegisteringError(n, "ConstructorDecl")
    }
  }

  def registerDecl(n : AGNode[JavaNodeKind], decl : AST.MethodDecl){
    n.kind match {
      case m @ Method() => m.decl = decl
      case m @ AbstractMethod() => m.decl = decl
      case _ => throwRegisteringError(n, "MethodDecl")
    }
  }

  def registerDecl(n : AGNode[JavaNodeKind], decl : AST.FieldDeclaration){
    n.kind match {
      case f @ Field() => f.decl = decl
      case _ => throwRegisteringError(n, "FieldDeclaration")
    }
  }

  def registerDecl(n : AGNode[JavaNodeKind], decl : AST.PrimitiveType){
    n.kind match {
      case f @ Primitive() => f.decl = decl
      case _ => throwRegisteringError(n, "PrimitiveType")
    }
  }
}
