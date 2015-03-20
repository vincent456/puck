package puck.graph

import puck.graph.transformations._
import puck.javaGraph.nodeKind.{MethodTypeHolder, NamedTypeHolder}

import scalaz._

import scala.language.implicitConversions
/**
 * Created by lorilan on 2/6/15.
 */
object ShowDG {

  //def showKind : Show[NodeKind] = Show.showFromToString
  //def showOperation : Show[Operation] = Show.showFromToString
  implicit def showFromToString[A] : Show[A] = Show.showFromToString[A]

  type CordBuilder[A] = (DependencyGraph, A) => Cord


  import Cord.stringToCord

  def name(g : DependencyGraph, n : DGNode) : String = {
    n match {
      case cn : ConcreteNode => cn.name
      case vn : VirtualNode =>
        vn.name(g)
    }
  }

  implicit def typeCord : CordBuilder[Type] = (dg, th) => th match {
    case NamedType(nid) => name(dg, dg.getNode(nid))
    case Tuple(types) =>
      types.map(t => typeCord(dg,t.asInstanceOf[Type]).toString).mkString("(", ", ", ")")
      //types.map(typeCord(dg,_)).fold
    case Arrow(in, out) =>Cord( typeCord(dg, in.asInstanceOf[Type]), " -> ", typeCord(dg, out.asInstanceOf[Type]))
  }

  implicit def typeHolderCord : CordBuilder[TypeHolder] = (dg, th) => th match {
    case NoType => ""
    case NamedTypeHolder(t) => Cord(" : ", typeCord(dg, t))
    case MethodTypeHolder(t) => Cord(" : ", typeCord(dg, t))
  }

  implicit def nodeIdCord : CordBuilder[NodeId] =
    (dg, nid) => nodeCord(dg, dg.getNode(nid))

  implicit def nodeCord : CordBuilder[DGNode] = (dg, n) =>
    n match {
      case n : ConcreteNode => Cord(s"${n.id} - ${n.kind} ${n.name}", typeHolderCord(dg, n.styp))
      case vn : VirtualNode => Cord(s"${vn.id} - ${vn.name(dg)}")
    }

  def nodeNameTypCord : CordBuilder[DGNode] =
    (dg, n) => n match {
      case cn : ConcreteNode => Cord(cn.name , typeHolderCord(dg, cn.styp))
      case _ => name(dg, n)
    }

  implicit def edgeCord : CordBuilder[DGEdge] =  (dg, e) =>
    Cord(e.kind.toString, "( " + nodeIdCord(dg, e.source), ", ", nodeIdCord(dg, e.target), ")")

  implicit def extremityCord : CordBuilder[Extremity] =
    (dg, e) => Cord(e.productPrefix, "(", nodeCord(dg, dg.getNode(e.node)), ")")

  implicit def transfoTargetCord : CordBuilder[TransformationTarget] = (dg, tgt) =>
    tgt match {
      case TTEdge(edge) => edgeCord(dg, edge)
      case TTRedirection(edge, exty) =>
        val ecord = edgeCord(dg, edge)
        val xcord = extremityCord(dg, exty)
        Cord(tgt.productPrefix, "(", ecord ,",", xcord ,")")

      case TTTypeRedirection(typed, typ, oldUsed, newUsed) =>
        val ntyped = dg.getNode(typed).toString()
        val nold = dg.getNode(oldUsed).toString()
        val nnew = dg.getNode(newUsed).toString()
        Cord(tgt.productPrefix, "(", ntyped ,",", typ.toString ,
          ",", nold , ",", nnew ,")")
      case _ => tgt.toString
    }

  implicit def transformationtCord : CordBuilder[Transformation] = (dg, tf) =>
    tf.target match {
      case TTCNode(_) | TTEdge(_) =>
        Cord(tf.operation.productPrefix, "(", transfoTargetCord(dg, tf.target),")")
      case _ =>  transfoTargetCord(dg, tf.target)
    }

  def showDG[A](sg : Option[DependencyGraph] = None)
                            (implicit s : CordBuilder[A]): Show[A] = sg match {
    case None => Show.showFromToString
    case Some(g) => new Show[A] {
      override def show(a : A) = s(g, a)
    }
  }

  implicit def graphToOptionGraph(g : DependencyGraph) : Option[DependencyGraph] = Some(g)

}
