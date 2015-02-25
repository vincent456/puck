package puck

/**
 * Created by lorilan on 29/10/14.
 */
package object javaGraph {
  val defaultPackageName = "<default package>"

  def filterPackageName(name: String) = name match {
    case "" => defaultPackageName
    case _ => name
  }
}
