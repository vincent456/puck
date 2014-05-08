package puck.graph

import scala.collection.mutable
import scala.collection.JavaConversions.collectionAsScalaIterable
/**
 * Created by lorilan on 05/05/14.
 */
class AccessGraph {

  private[graph] val nodesById : mutable.Map[Int, AGNode] = new mutable.HashMap[Int, AGNode]()
  private var id : Int = 1
  val root : AGNode = new AGNode(this, 0, "root", AGRoot(), None)

  private[graph] val nodesByName : mutable.Map[String, AGNode] = new mutable.HashMap[String, AGNode]()

  def attachNodesWithoutContainer() {
    for((_, n) <- nodesById){
      n.getContainer match {
        case None => ()
        case Some(_) => root addContent n
      }
    }
  }

  def getNode(fullName:String) = nodesByName(fullName)
  def getNode(id: Int) = nodesById(id)

  def addNode(fullName: String, localName:String, kind: NodeKind, `type`: Option[Type]): AGNode =
    nodesByName get fullName match{
      case None => id = id + 1
        val n = new AGNode(this, id, localName, kind,`type`)
        this.nodesByName += ((fullName, n))
        this.nodesById += ((id, n))
        n
      case Some(n) => n /* check that the kind and type is indeed the same ??*/
    }

  def addPackageNode(fullName: String, localName:String) : AGNode =
    addNode(fullName, localName, java.JavaNodeKind.`package`, None)

  def addPackage(p : String): AGNode = {
    val fp = AccessGraph.filterPackageName(p)
    val path = fp split "[.]" //TODO test split
    if (path.length == 0)
      addPackageNode(fp, fp)
    else {
      val (_, n):(mutable.StringBuilder, AGNode) = path.foldLeft(new mutable.StringBuilder(), root){
        (sb_nodeParent:(mutable.StringBuilder, AGNode), p:String) => sb_nodeParent match {
          case (sb, nodeParent) =>
            sb append p
            val n = addPackageNode(sb.toString(), p)
            nodeParent addContent n
            sb append "."
            (sb, n)
        }
      };
      n
    }
  }


  def addApiTypeNode(td: AST.TypeDecl): AGNode = {

    val packageNode = addPackage(td.compilationUnit().getPackageDecl)
    val tdNode = td.buildAGNode(this)

    for (use <- td.uses()) {
      tdNode.addUser(use.buildAGNode(this))
    }

    packageNode.addContent(tdNode)
    tdNode
  }


  def addApiNode(program : AST.Program, nodeKind : String, `type` : String, bodydeclName: String){
    println("trying to add type "+`type` + " " + bodydeclName+ " ... ")
    val td = program  findType `type`
    if(td == null){
      System.err.println(`type` + " not found")
      return
    }

    val tdNode = addApiTypeNode(td)

    def addBodyDecl(bd : AST.BodyDecl){
      if(bd == null)
        System.err.println("Method or constructor" + bodydeclName + " not found in the program ...")
      else
        tdNode addContent (bd buildAG this)
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
        val packageNode = addPackage(bd.hostBodyDecl().compilationUnit().getPackageDecl())

      val bdNode = bd buildAGNode this
      val strNode = addNode(bd.fullName()+literal, literal, java.JavaNodeKind.literal, Some(java.Primitive.string))

      /*
        this is obviously wrong: TODO FIX
      */
      packageNode addContent strNode
      strNode addUser bdNode
    }
  }
}

object AccessGraph{

  val defaultPackageName = "<default package>"

  def filterPackageName(name:String) = name match {
    case "" => defaultPackageName
    case _ => name
  }

}