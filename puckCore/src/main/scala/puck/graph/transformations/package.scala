package puck.graph


package object transformations {

  implicit class RecordingOps(val record : Recording) extends AnyVal {

    def redo(g : DependencyGraph) : DependencyGraph =
      record.reverse.foldLeft(g)((g0, t) => t.redo(g0))

    def mapTransformation(f : Transformation => Transformation) : Recording =
      record map {
        case t : Transformation => f(t)
        case r => r
      }

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
                       oldUsee: NodeId,
                       newUsee : NodeId) : Recording =
      Transformation(Regular, TypeRedirection(typed, oldUsee, newUsee)) +: record

    def addAbstraction(impl : NodeId, abs : Abstraction) : Recording =
      Transformation(Regular, AbstractionOp(impl, abs)) +: record

    def removeAbstraction(impl : NodeId, abs : Abstraction) : Recording =
      Transformation(Reverse, AbstractionOp(impl, abs)) +: record

    def addTypeDependency( typeUse : NodeIdP,
                           typeMemberUse :  NodeIdP) : Recording =
      Transformation(Regular, TypeDependency(typeUse, typeMemberUse)) +: record


    def removeTypeDependency( typeUse : NodeIdP,
                              typeMemberUse :  NodeIdP) : Recording =
      Transformation(Reverse, TypeDependency(typeUse, typeMemberUse)) +: record

  }
}
