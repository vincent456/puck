package puck.graph

import puck.graph

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

class UsesDependencyMap(val user : AGNode,
                        val keyType : DependencyStatus) {
  private [this] val content : mutable.Map[AGNode, mutable.Set[AGEdge]] = mutable.Map()

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

  def get(key : AGNode) : Option[Iterable[AGEdge]] = content.get(key)

  def apply(key : AGNode) : Iterable[AGEdge] = content(key)

  def +=(usee : AGNode, dependency : AGEdge) = {

    content get usee match {
      case None =>
        content += (usee -> mutable.Set(dependency))
      case Some(s) =>
        content += (usee -> s.+=(dependency))
    }

  }

  def -=(usee : AGNode, dependency : AGEdge){
    val dependencies = content(usee)

    dependencies.remove(dependency)
    if(dependencies.isEmpty)
      content.remove(usee)

  }

  def isEmpty = content.isEmpty
}
