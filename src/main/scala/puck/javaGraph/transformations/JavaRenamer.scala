package puck.javaGraph.transformations

import puck.graph._
import puck.graph.transformations.rules.Renamer

object JavaRenamer extends Renamer{


  override def apply
  ( g : DependencyGraph,
    id : NodeId,
    newName : String) : DependencyGraph = {

    val n = g.getNode(id)
    val kindType =  g.kindType(n)

    def dual =
      kindType match {
        case TypeConstructor =>
          Set(g.container(n.id).get)
        case TypeDecl =>
          g.content(n.id).filter{nid => g.kindType(nid) == TypeConstructor}
        case _ => sys.error("Cannot happen")
      }


    kindType match {
      case TypeConstructor
        | TypeDecl => dual.foldLeft(g.setName(id, newName)){
        (g0, nid) => g0.setName(nid, newName)
      }

      case _ => super.apply(g, id, newName)
    }
  }
}
