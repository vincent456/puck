package puck.graph

import scala.collection.mutable
import scala.collection.JavaConversions.collectionAsScalaIterable
/**
 * Created by lorilan on 05/05/14.
 */

object AccessGraph{

  val defaultPackageName = "<default package>"

  def filterPackageName(name:String) = name match {
    case "" => defaultPackageName
    case _ => name
  }

  val rootId = 0

}

class AccessGraph {

  private[graph] val nodesById : mutable.Map[Int, AGNode] = new mutable.HashMap[Int, AGNode]()
  private var id : Int = 1
  val root : AGNode = new AGNode(this, AccessGraph.rootId, "root", AGRoot(), None)

  private[graph] val nodesByName : mutable.Map[String, AGNode] = new mutable.HashMap[String, AGNode]()

  /*def list(){
    nodesByName.foreach((kn) => {
      val (key, node) = kn
      println(" ******** " + key + "( "+ node.id +" ) ******** ")
      println(" contained by " + node.container)
      println(" contains " + node.content)
      println()
    })
  }*/

  def attachNodesWithoutContainer() {
    for((_, n) <- nodesById){
      n.container match {
        case None => root content_+= n
        case Some(_) => ()
      }
    }
  }

  def getNode(fullName:String) = nodesByName(fullName)
  def getNode(id: Int) = nodesById(id)

  def addNode(fullName: String, localName:String, kind: NodeKind, `type`: Option[Type]): AGNode = {
    val unambiguousFullName = `type` match {
      case None => fullName
      case Some(t) => fullName + " : " + t
    }
    nodesByName get unambiguousFullName match {
      case None => id = id + 1
        val n = new AGNode(this, id, localName, kind, `type`)
        this.nodesByName += ((unambiguousFullName, n))
        this.nodesById += ((id, n))
        n
      case Some(n) => n /* check that the kind and type is indeed the same ??*/
    }
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
            nodeParent content_+=  n
            sb append "."
            (sb, n)
        }
      }
      n
    }
  }


  def addApiTypeNode(td: AST.TypeDecl): AGNode = {

    val packageNode = addPackage(td.compilationUnit().getPackageDecl)
    val tdNode = td.buildAGNode(this)

    for (use <- td.uses()) {
      tdNode.users_+=(use.buildAGNode(this))
    }

    packageNode content_+= tdNode
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
        tdNode content_+= (bd buildAG this)
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
      packageNode content_+= strNode
      strNode users_+= bdNode
    }
  }
}

