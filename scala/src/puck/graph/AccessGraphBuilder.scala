package puck.graph

import scala.collection.mutable

/**
 * Created by lorilan on 05/05/14.
 */
class AccessGraphBuilder {

  private val graph : AccessGraph = new AccessGraph()
  private var id : Int = 0

  val nodes : mutable.Map[String, AGNode] = new mutable.HashMap[String, AGNode]()

  def addNode(fullName: String, localName:String, kind: NodeKind.Value, `type`: Option[Type]): AGNode =
    nodes get fullName match{
      case None => id = id + 1
        val n = new AGNode(graph, id, localName, kind,`type`)
        this.nodes += ((fullName, n))
        graph.nodes += ((id, n))
        n
      case Some(n) => n /* check that the kind and type is indeed the same ??*/
    }

  def addPackageNode(fullName: String, localName:String) : AGNode =
    addNode(fullName, localName, NodeKind.Package, None)

  def addPackage(p : String): AGNode = {
    val fp = AccessGraphBuilder.filterPackageName(p)
    val path = fp split "[.]" //TODO test split
    if (path.length == 0)
      addPackageNode(fp, fp)
    else {
      val (_, n):(mutable.StringBuilder, AGNode) = path.foldLeft(new mutable.StringBuilder(), null : AGNode){
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
}

object AccessGraphBuilder{

  val defaultPackageName = "<default package>"

  def filterPackageName(name:String) = name match {
    case "" => defaultPackageName
    case _ => name
  }

  def addIsa(sub : AGNode, sup: AGNode){
    sub.addSuperType(sup)
    sup.addSubType(sub)
  }

}
