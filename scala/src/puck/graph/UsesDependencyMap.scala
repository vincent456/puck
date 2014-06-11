package puck.graph

import scala.collection.mutable

/**
 * Created by lorilan on 11/06/14.
 */
class UsesDependencyMap(val user : AGNode,
                        val keyType : String,
                        val contentType : String) {
  private [this] val content : mutable.Map[AGNode, mutable.Set[AGEdge]] = mutable.Map()

  override def toString : String = {
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

  def ++=(usee : AGNode, dependencies: mutable.Set[AGEdge]){
    content get usee match {
      case None =>
        content += (usee -> dependencies)
      case Some(s) =>
        content += (usee -> s.++=(dependencies))
    }
    dependencies.foreach{ _.create() }
    usee.users_+=(user)
  }

  def -=(usee : AGNode){
    content get usee match {
      case None => ()
      case Some(dependencies) =>
        dependencies.foreach{ _.delete() }
        content.remove(usee)
    }
    usee users_-= user

  }

  def -=(usee : AGNode, dependency : AGEdge){
    val dependencies = content(usee)
    dependencies.remove(dependency)
    dependency.delete()
    if(dependencies.isEmpty)
      content.remove(usee)
    usee.users_-=(user)
  }

  def isEmpty = content.isEmpty
}
