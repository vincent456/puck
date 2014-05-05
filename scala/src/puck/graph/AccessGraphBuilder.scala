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
    val fp = filterPackageName(p)
    val path = fp split "[.]" //TODO test split
    if (path.length == 0)
      addPackageNode(fp, fp)
    else {
      val (_, n) = path.foldLeft[mutable.StringBuilder, AGNode](new mutable.StringBuilder(), null){
        ((sb, nodeParent), p) =>
        sb append p
        val n = addPackageNode(sb.toString(), p)
        nodeParent addContent n
        sb append "."
        (sb, n)
      }
      n
    }
  }

  //def addCompilationUnit(c: AST.CompilationUnit)

  private def addTypeDecl(td : AST.TypeDecl) : AGNode = {
    val tdNode = if(td.isInstanceOf[AST.ClassDecl])
             addClassDecl()
  }


}

object AccessGraphBuilder{

  val defaultPackageName = "<default package>"

  def filterPackageName(name:String) = name match {
    case "" => defaultPackageName
    case _ => name
  }

}
