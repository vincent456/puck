package puck.javaGraph.transformations

import puck.graph._
import puck.graph.constraints.SupertypeAbstraction
import puck.graph.transformations.rules.Renamer
import puck.javaGraph.nodeKind.{MethodKind, TypeKind, Constructor}

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
          Set(g.container(id).get)
        case TypeDecl =>
          g.content(id).filter{nid => g.kindType(nid) == TypeConstructor}
        case _ => sys.error("Cannot happen")
      }

    def methodHierarchy : Set[NodeId] = {
      def aux(border : Set[NodeId], hierarchy : Set[NodeId]) : Set[NodeId] =
        if(border.isEmpty) hierarchy
        else {
          val nid = border.head
          val abs = g.abstractions(nid).filter{
            case (_, SupertypeAbstraction) => true
            case _ => false
          }.map(_._1)

          val subClasses = g.subTypes(g.container(nid).get)

          val allAbs = subClasses.foldLeft(abs){
            (acc, classId) =>
              g.content(classId).filter {
                subMethId => g.isAbstraction(subMethId, nid, SupertypeAbstraction)
              } union acc
          }

          val newBorder = allAbs.foldLeft(Set[NodeId]()){
            (border0, absId) =>
              if(hierarchy contains absId) border0
              else border0 + absId
          } union border.tail
          aux(newBorder, hierarchy + border.head)
        }

      aux(Set(id), Set())
    }


    n.kind match {
      case Constructor
        | _ : TypeKind => dual.foldLeft(g.setName(id, newName)){
        (g0, nid) => g0.setName(nid, newName)
      }
      case _ : MethodKind =>
        methodHierarchy.foldLeft(g){
          (g0, nid) => g0.setName(nid, newName)
        }

      case _ => super.apply(g, id, newName)
    }
  }
}
