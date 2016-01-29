package puck.graph

import puck.graph.constraints.ShowConstraints
import puck.graph.transformations._

import scalaz.{Cord, Show}


import scala.language.implicitConversions

object ShowDG extends ShowConstraints{

  implicit def showFromToString[A] : Show[A] = Show.showFromToString[A]



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

  implicit def nodeIdPCord : CordBuilder[NodeIdP] =
    {case (dg, (nid1, nid2)) => Cord("Edge(", nodeIdCord(dg,nid1), ", ", nodeIdCord(dg,nid2), ")")}


  implicit def nodeCord : CordBuilder[DGNode] = (dg, n) =>
    n match {
      case n : ConcreteNode =>
        val name =
          if(n.kind.kindType == ValueDef)
            dg.container(n.id) map {
              dg.getConcreteNode(_).name + DependencyGraph.scopeSeparator + n.name
            } getOrElse "OrphanDefinition"
          else n.name
        Cord(s"${n.id} - ${n.kind} $name", typeHolderCord(dg, dg.styp(n.id)))
      case vn : VirtualNode => Cord(s"${vn.id} - ${vn.name(dg)}")
    }


  def nodeNameTypCord : CordBuilder[DGNode] =
    (dg, n) => n match {
      case cn : ConcreteNode => Cord(cn.name , typeHolderCord(dg, dg.styp(cn.id)))
      case _ => n.name(dg)
    }

  def fullNameEdgeCord : CordBuilder[DGEdge] =  (g, e) =>
    Cord(s"${e.kind}(${e.source} - ${g.fullName(e.source)}, ${e.target} - ${g.fullName(e.target)})")


  implicit def edgeCord : CordBuilder[DGEdge] =  (dg, e) =>
    Cord(e.kind.toString, "( " + nodeIdCord(dg, e.source), ", ", nodeIdCord(dg, e.target), ")")
  implicit def brCord : CordBuilder[(DGEdge,DGEdge)] =
  {case (dg, (u1, u2)) => Cord("Edge(", edgeCord(dg,u1), ", ", edgeCord(dg,u2), ")")}


  implicit def extremityCord : CordBuilder[Extremity] =
    (dg, e) => Cord(e.productPrefix, "(", nodeCord(dg, dg.getNode(e.node)), ")")

  implicit def transfoTargetCord : CordBuilder[Operation] = (dg, tgt) =>
    tgt match {
      case CNode(n) => nodeCord(dg, n)
      case VNode(n) =>nodeCord(dg, n)
      case Edge(edge) => edgeCord(dg, edge)
      case RedirectionOp(edge, exty) =>
        val ecord = edgeCord(dg, edge)
        val xcord = extremityCord(dg, exty)
        Cord(tgt.productPrefix, "(", ecord ,",", xcord ,")")
      case TypeDependency((n1,n2), (n3,n4)) =>
        Cord(tgt.productPrefix, "(Uses(",  nodeIdCord(dg, n1), ", ", nodeIdCord(dg,n2),
          "),Uses(", nodeIdCord(dg, n3), ", ", nodeIdCord(dg,n4) ,"))")

      case TypeChange(typed, soldT, snewT) =>
        val typedCord = nodeIdCord(dg, typed)
        val soldTcord : Cord = soldT map (typeCord(dg,_)) getOrElse "NoType"
        val snewTcord : Cord = snewT map (typeCord(dg,_)) getOrElse "NoType"
        Cord("TypeChange(", typedCord, ", ", soldTcord, ", " ,snewTcord ,")")

      case ChangeTypeBinding((tUse, tmUse), exty) =>
        val tUseCord = nodeIdPCord(dg, tUse)
        val tmUseCord = nodeIdPCord(dg, tmUse)
        Cord("ChangeTypeBinding((", tUseCord, ", ", tmUseCord,"),", exty.productPrefix,
          "(", nodeIdPCord(dg, exty.edge) ,")")
      case _ => tgt.toString
    }

  val directAddRm : Direction => String = {
    case Regular => "Add"
    case Reverse => "Remove"
  }


  implicit def abstractionCordBuilder : CordBuilder[Abstraction] = (dg, a) =>
    a match {
      case AccessAbstraction(nid, policy) =>
        Cord("AccessAbstraction(", dg.getNode(nid).name , ", ", policy.toString,")")
      case ReadWriteAbstraction(rid, wid) =>
        val n1 = rid map (dg.getNode(_).name) toString ()
        val n2 = wid map (dg.getNode(_).name) toString ()
        Cord("ReadWriteAbstraction(", n1 , ", ", n2,")")
    }

  implicit def transformationtCord : CordBuilder[Recordable] = (dg, r) =>
    r match {
      case tf : Transformation =>
        if(Transformation.isAddRmOperation(tf.operation))
          Cord(directAddRm(tf.direction), "(", transfoTargetCord(dg, tf.operation),")")
        else transfoTargetCord(dg, tf.operation)

      case MileStone => Cord(r.toString)
      case Comment(msg) => Cord("***", msg, "***")
    }


  implicit class DGShowOp[A](val p : (DependencyGraph, A)) extends AnyVal {
    def shows(implicit cb : CordBuilder[A]) : String =
      cb(p._1, p._2).toString()

    def println(implicit cb : CordBuilder[A]) : Unit = System.out.println(shows)
  }
}
