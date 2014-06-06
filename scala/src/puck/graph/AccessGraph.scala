package puck.graph

import scala.collection.mutable
import scala.collection.JavaConversions.collectionAsScalaIterable

/**
 * Created by lorilan on 05/05/14.
 */

object AccessGraph {

  val defaultPackageName = "<default package>"

  def filterPackageName(name:String) = name match {
    case "" => defaultPackageName
    case _ => name
  }

  val rootId = 0

}

class AccessGraph (nodeBuilder : AGNodeBuilder) {
//extends Iterable[AGNode]{ //is NOT iterable (see scala doc for requirements) but
// has a valid iterator + implicit conversion in puck.graph package object

  println("Node builder : " + nodeBuilder.getClass)

  private [puck] val nodesById : mutable.Map[Int, AGNode] = mutable.Map()
  private [puck] val nodesByName : mutable.Map[String, AGNode] = mutable.Map()
  /*private[graph] val predefTypes : mutable.Map[String, Type] = mutable.Map()
  def predefType(name : String ) = predefTypes(name)*/

  private var id : Int = 1
  val root : AGNode = nodeBuilder(this, AccessGraph.rootId, "root", AGRoot())

  def nodeKinds = nodeBuilder.kinds

  def iterator = root.iterator

  def violations : List[AGEdge] = {
     this.foldLeft(List[AGEdge]()){
      (acc: List[AGEdge], n :AGNode) =>
      n.wrongUsers.map{AGEdge.uses(_, n)} :::(
        if(n.isWronglyContained )
          AGEdge.contains(n.container_!, n) :: acc
        else acc)
    }
  }

  /*def violations : Set[Violation] = {
    this.foldLeft(Set[Violation]()){(acc: Set[Violation], n :AGNode) => n.targetingViolations(acc)}
  }*/

  def discardConstraints() {
    this.foreach(_.discardConstraints())
  }

  def printConstraints(){
    this.foreach(n => print(n.constraintsString))
  }

  def printUsesDependancies(){
    this.foreach { node =>
      if (node.hasPrimaryUses)
        println(node.primaryUsesString)
      if (node.hasSideUses)
        println(node.sideUsesString)
    }
  }
  /*def list(){
    nodesByName.foreach((kn) => {
      val (key, node) = kn
      println(" ******** " + key + "( "+ node.id +" ) ******** ")
      println(" contained by " + node.container)
      println(" contains " + node.content)
      println()
    })
  }*/
  def list(){
    nodesByName.foreach((kn) => println(kn._1))
  }

  def attachNodesWithoutContainer() {
    for((_, n) <- nodesById){
      n.container match {
        case None => root content_+= n
        case Some(_) => ()
      }
    }
  }

  /*
    throw exception on failure
   */
  def apply(fullName:String) : AGNode= nodesByName(fullName)
  def apply(id: Int) : AGNode = nodesById(id)

  def getNode(fullName:String) : Option[AGNode] = nodesByName get fullName
  def getNode(id: Int) : Option[AGNode] = nodesById get id

  def addNode(localName:String, kind: NodeKind) : AGNode = {
    id = id + 1
    val n = nodeBuilder(this, id, localName, kind)
    this.nodesById += (id -> n)
    n
  }

  def addNode(fullName: String, localName:String, kind: NodeKind): AGNode = {
    val unambiguousFullName = nodeBuilder.makeKey(fullName, localName, kind)
    nodesByName get unambiguousFullName match {
      case None =>
        val n = addNode(localName, kind)
        this.nodesByName += (unambiguousFullName -> n)
        n
      case Some(n) => n /* check that the kind and type is indeed the same ??*/
    }
  }

  def addNode(fullName: String, localName:String): AGNode =
    addNode(fullName, localName, VanillaKind())

  def addPackageNode(fullName: String, localName:String) : AGNode =
    addNode(fullName, localName, puck.javaAG.JavaNodeKind.`package`)

  def addPackage(p : String): AGNode = {
    val fp = AccessGraph.filterPackageName(p)
    val path = fp split "[.]"
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
      val packageNode = addPackage(bd.hostBodyDecl.compilationUnit.getPackageDecl)

      val bdNode = bd buildAGNode this
      val strNode = addNode(bd.fullName()+literal, literal,
        puck.javaAG.JavaNodeKind.literal(NamedType(this("java.lang.String"))))

      /*
        this is obviously wrong: TODO FIX
      */
      packageNode content_+= strNode
      strNode users_+= bdNode
    }
  }
}

