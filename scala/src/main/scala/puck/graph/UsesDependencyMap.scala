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

class UsesDependencyMap[Kind <: NodeKind[Kind]](val keyType : DependencyStatus)
  extends Iterable[(AGEdge[Kind],  Iterable[AGEdge[Kind]])]{

  type NodeType = AGNode[Kind]
  private [this] val content : mutable.Map[AGEdge[Kind], mutable.Set[AGEdge[Kind]]] = mutable.Map()

  def iterator = content.iterator

  override def toString() : String = {
    val contentType = keyType.other.contentString
    content.map{
      case (key, values) =>
        val contentStr =
          if(values.isEmpty){"(no "+ contentType+")\n"}
          else {
            values.mkString("\n" + contentType + " :\n\t", "\n\t", "\n")
          }
        keyType + " : (" + key + ")" + contentStr
    }.mkString("")
  }

  def get(key : AGEdge[Kind]) : Option[Iterable[AGEdge[Kind]]] = content.get(key)

  def getOrElse(key : AGEdge[Kind], default : Iterable[AGEdge[Kind]]) = content.getOrElse(key, default)
  def getOrEmpty(key : AGEdge[Kind]) = content.getOrElse(key, Iterator.empty)

  def apply(key : AGEdge[Kind]) : Iterable[AGEdge[Kind]] = content(key)

  def +=(key : AGEdge[Kind], dependency : AGEdge[Kind]) = {

    if(key == dependency)
      sys.error("WTF !")

    content get key match {
      case None =>
        content += (key -> mutable.Set(dependency))
      case Some(s) => s += dependency
        //content += (usee -> s.+=(dependency))
    }

  }

  def -=(key : AGEdge[Kind], dependency : AGEdge[Kind]){
    val dependencies = content(key)

    dependencies.remove(dependency)
    if(dependencies.isEmpty)
      content.remove(key)

  }
}
