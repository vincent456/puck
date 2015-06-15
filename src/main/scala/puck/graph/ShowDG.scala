package puck.graph

import puck.graph.transformations._

import scalaz.{Cord, Show}


import scala.language.implicitConversions

object ShowDG {

  //def showKind : Show[NodeKind] = Show.showFromToString
  //def showOperation : Show[Operation] = Show.showFromToString
  implicit def showFromToString[A] : Show[A] = Show.showFromToString[A]

  type CordBuilder[A] = (DependencyGraph, A) => Cord

  def tailRecTypeCord
  (dg : DependencyGraph, sep : List[String], end : List[String], builder : StringBuilder, lt : List[List[Type]]) : String =
    if (lt.isEmpty) builder.toString()
    else lt.head match {
      case Nil => tailRecTypeCord(dg, sep.tail, end.tail, builder append end.head, lt.tail)
      case NamedType(nid) :: tl =>
        val sep0 =
          if(tl.nonEmpty) sep.head
          else ""

        tailRecTypeCord(dg, sep, end, builder.append(dg.getNode(nid).name(dg) + sep0), tl :: lt.tail )
      case Tuple(types) :: tl =>
        tailRecTypeCord(dg, ", " :: sep, (")" + sep.head) :: end, builder append "(", types :: tl :: lt.tail )
      case Arrow(in, out) :: tl =>
        tailRecTypeCord(dg, " -> " :: sep, "" :: end, builder, List(in, out) :: tl :: lt.tail )
    }

  implicit def typeCord : CordBuilder[Type] = (dg, t) => tailRecTypeCord(dg, List(""), List(""), new StringBuilder, List(List(t)))
//  implicit def typeCord : CordBuilder[Type] = (dg, th) => th match {
//    case NamedType(nid) => dg.getNode(nid).name(dg)
//    case Tuple(types) => types.map(t => typeCord(dg,t).toString()).mkString("(", ", ", ")")
//      //types.map(typeCord(dg,_)).fold
//    case Arrow(in, out) => Cord( typeCord(dg, in), " -> ", typeCord(dg, out))
//  }

  implicit def typeHolderCord : CordBuilder[Option[Type]] = (dg, th) => th match {
    case None => ""
    case Some(t) => Cord(" : ", typeCord(dg, t))
  }

  implicit def nodeIdCord : CordBuilder[NodeId] =
    (dg, nid) => nodeCord(dg, dg.getNode(nid))

  implicit def nodeCord : CordBuilder[DGNode] = (dg, n) =>
    n match {
      case n : ConcreteNode => Cord(s"${n.id} - ${n.kind} ${n.name}", typeHolderCord(dg, n.styp))
      case vn : VirtualNode => Cord(s"${vn.id} - ${vn.name(dg)}")
    }


  def nodeNameCord : CordBuilder[DGNode] =
    (dg, n) => n.name(dg)

  def nodeNameTypCord : CordBuilder[DGNode] =
    (dg, n) => n match {
      case cn : ConcreteNode => Cord(cn.name , typeHolderCord(dg, cn.styp))
      case _ => n.name(dg)
    }

  implicit def edgeCord : CordBuilder[DGEdge] =  (dg, e) =>
    Cord(e.kind.toString, "( " + nodeIdCord(dg, e.source), ", ", nodeIdCord(dg, e.target), ")")

  implicit def extremityCord : CordBuilder[Extremity] =
    (dg, e) => Cord(e.productPrefix, "(", nodeCord(dg, dg.getNode(e.node)), ")")

  implicit def transfoTargetCord : CordBuilder[Operation] = (dg, tgt) =>
    tgt match {
      case Edge(edge) => edgeCord(dg, edge)
      case RedirectionOp(edge, exty) =>
        val ecord = edgeCord(dg, edge)
        val xcord = extremityCord(dg, exty)
        Cord(tgt.productPrefix, "(", ecord ,",", xcord ,")")

//      case TypeRedirection(typed, oldUsed, newUsed) =>
//        val ntyped = dg.getNode(typed).toString
//        val nold = dg.getNode(oldUsed).toString
//        val nnew = dg.getNode(newUsed).toString
//        Cord(tgt.productPrefix, "(", ntyped ,",", dg.getConcreteNode(typed).styp.toString ,
//          ",", nold , ",", nnew ,")")
      case _ => tgt.toString
    }

  val directAddRm : Direction => String = {
    case Regular => "Add"
    case Reverse => "Remove"
  }
  val addRmOperation : Operation => Boolean = {
    case _ : VNode
      | _ : CNode
      | _ : Edge
      | _ : AbstractionOp => true
    case _ => false
  }

  implicit def transformationtCord : CordBuilder[Recordable] = (dg, r) =>
    r match {
      case tf : Transformation =>
        if(addRmOperation(tf.operation))
          Cord(directAddRm(tf.direction), "(", transfoTargetCord(dg, tf.operation),")")
        else transfoTargetCord(dg, tf.operation)

      case MileStone => Cord(r.toString)
      case Comment(msg) => Cord("***", msg, "***")
    }


  def showDG[A](sg : Option[DependencyGraph] = None)
                            (implicit s : CordBuilder[A]): Show[A] = sg match {
    case None => Show.showFromToString
    case Some(g) => Show.show(s(g, _))
  }

  implicit def graphToOptionGraph(g : DependencyGraph) : Option[DependencyGraph] = Some(g)

}
