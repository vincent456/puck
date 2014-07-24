package puck.graph



import scala.collection.mutable

/**
 * Created by lorilan on 11/06/14.
 */
sealed abstract class DependencyStatus{
  def other : DependencyStatus
  def contentString : String
}
case class Dominant() extends DependencyStatus {
  override def toString = "primary"
  def contentString = "primaries"
  def other = Dominated()
}
case class Dominated() extends DependencyStatus {
  override def toString = "secondary"
  def contentString = "secondaries"
  def other = Dominant()
}

class UsesDependencyMap[Kind <: NodeKind[Kind]](val user : AGNode[Kind],
                        val keyType : DependencyStatus) {
  type NodeType = AGNode[Kind]
  private [this] val content : mutable.Map[NodeType, mutable.Set[AGEdge[Kind]]] = mutable.Map()

  override def toString : String = {
    val contentType = keyType.other.contentString
    content.map{
      case (key, values) =>
        val contentStr =
          if(values.isEmpty){"(no "+ contentType+")\n"}
          else {
            values.mkString("\n" + contentType + " :\n\t", "\n\t", "\n")
          }
        keyType + " : (" + this + ", " + key + ")" + contentStr
    }.mkString("")
  }

  def get(key : NodeType) : Option[Iterable[AGEdge[Kind]]] = content.get(key)

  def getOrElse(key : NodeType, default : Iterable[AGEdge[Kind]]) = content.getOrElse(key, default)

  def apply(key : NodeType) : Iterable[AGEdge[Kind]] = content(key)

  def +=(usee : NodeType, dependency : AGEdge[Kind]) = {

    content get usee match {
      case None =>
        content += (usee -> mutable.Set(dependency))
      case Some(s) => s += dependency
        //content += (usee -> s.+=(dependency))
    }

  }

  def -=(usee : NodeType, dependency : AGEdge[Kind]){
    val dependencies = content(usee)

    dependencies.remove(dependency)
    if(dependencies.isEmpty)
      content.remove(usee)

  }

  def isEmpty = content.isEmpty
}
