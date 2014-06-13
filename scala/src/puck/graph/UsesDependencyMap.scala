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

  def apply(key : AGNode) : Option[Iterable[AGEdge]] = content.get(key)

  def +=(usee : AGNode, dependencies : AGEdge) = {
    this.++=(usee, mutable.Set(dependencies))
  }

  def order(usee : AGNode, dependency : AGEdge) : (AGEdge, AGEdge) = {
    keyType match {
      case Dominant() => (AGEdge.uses(user, usee), dependency)
      case Dominated() => (dependency, AGEdge.uses(usee, user))
    }
  }

  def ++=(usee : AGNode, dependencies: mutable.Set[AGEdge]){
    content get usee match {
      case None =>
        content += (usee -> dependencies)
      case Some(s) =>
        content += (usee -> s.++=(dependencies))
    }
    dependencies.foreach{ e =>
      e.create()
      val (dominant, dominated) = order(usee, e)
      user.graph.transformations.addEdgeDependency(dominant, dominated)
    }
    usee.users_+=(user)
  }

  def -=(usee : AGNode){
    content get usee match {
      case None => ()
      case Some(dependencies) =>
        dependencies.foreach{ e =>
          e.delete()
          val (dominant, dominated) = order(usee, e)
          user.graph.transformations.removeEdgeDependency(dominant, dominated)
        }
        content.remove(usee)
    }
    usee users_-= user

  }

  def -=(usee : AGNode, dependency : AGEdge){
    val dependencies = content(usee)
    dependencies.remove(dependency)
    dependency.delete()

    val (dominant, dominated) = order(usee, dependency)
    user.graph.transformations.removeEdgeDependency(dominant, dominated)

    if(dependencies.isEmpty)
      content.remove(usee)
    usee.users_-=(user)
  }

  def isEmpty = content.isEmpty
}
