package puck.graph

import puck.graph.constraints.AbstractionPolicy


package object transformations {

  implicit class RecordingOps(val record : Recording) extends AnyVal {

//    override def equals(obj : Any) : Boolean = obj match {
//      case r : Recording => r() == record
//      case _ => false
//    }

    def redo(g : DependencyGraph) : DependencyGraph =
      record.reverse.foldLeft(g)((g0, t) => t.redo(g0))

    def mapTransformation(f : Transformation => Transformation) : Recording =
      record map {
        case t : Transformation => f(t)
        case r => r
      }


    //override def iterator: Iterator[Transformation] = record.reverseIterator
    /* def nonEmpty = record.nonEmpty
     def size = record.size*/

    /*  def addNode(id : NIdT, name : String, kind : NodeKind, styp: TypeHolder, mutable : Boolean) : RecT =
        Transformation(Add, TTNode(id, name, kind, styp, mutable)) +: this

      def removeNode(id : NIdT, name : String, kind : NodeKind, styp: TypeHolder, mutable : Boolean) : RecT =
        Transformation(Remove, TTNode(id, name, kind, styp, mutable)) +: this*/

    def comment(msg : String) : Recording =
      Comment(msg) +: record

    def mileStone : Recording = MileStone +: record

    def addConcreteNode(n : ConcreteNode) : Recording =
      Transformation(Regular, CNode(n)) +: record

    def addVirtualNode(n : VirtualNode) : Recording =
      Transformation(Regular, VNode(n)) +: record


    def changeNodeName(nid : NodeId, oldName : String, newName : String) : Recording =
      Transformation(Regular, ChangeNodeName(nid, oldName, newName)) +: record

    def removeConcreteNode(n : ConcreteNode) : Recording =
      Transformation(Reverse, CNode(n)) +: record

    def removeVirtualNode(n : VirtualNode) : Recording =
      Transformation(Reverse, VNode(n)) +: record

    def addEdge(edge : DGEdge) : Recording =
      Transformation(Regular, Edge(edge)) +: record

    def removeEdge(edge : DGEdge) : Recording=
      Transformation(Reverse, Edge(edge)) +: record

    def changeEdgeTarget(edge : DGEdge, newTarget : NodeId, withMerge : Boolean) : Recording = {
      val red = if(withMerge) new RedirectionWithMerge(edge, Target(newTarget))
      else RedirectionOp(edge, Target(newTarget))
      Transformation(Regular, red) +: record
    }

    def changeEdgeSource(edge : DGEdge, newTarget : NodeId, withMerge : Boolean) : Recording = {
      val red = if(withMerge) new RedirectionWithMerge(edge, Source(newTarget))
      else RedirectionOp(edge, Source(newTarget))
      Transformation(Regular, red) +: record
    }
    def addTypeChange( typed : NodeId,
                       typ: Option[Type],
                       oldUsee: NodeId,
                       newUsee : NodeId) : Recording =
      Transformation(Regular, TypeRedirection(typed, typ, oldUsee, newUsee)) +: record

    def addAbstraction(impl : NodeId, abs : NodeId, absPolicy : AbstractionPolicy) : Recording =
      Transformation(Regular, Abstraction(impl, abs, absPolicy)) +: record

    def removeAbstraction(impl : NodeId, abs : NodeId, absPolicy : AbstractionPolicy) : Recording =
      Transformation(Reverse, Abstraction(impl, abs, absPolicy)) +: record

    def addTypeDependency( typeUse : (NodeId, NodeId),
                           typeMemberUse :  (NodeId, NodeId)) : Recording =
      Transformation(Regular, TypeDependency(typeUse, typeMemberUse)) +: record


    def removeTypeDependency( typeUse : (NodeId, NodeId),
                              typeMemberUse :  (NodeId, NodeId)) : Recording =
      Transformation(Reverse, TypeDependency(typeUse, typeMemberUse)) +: record

  }
}
