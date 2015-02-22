package puck.graph

import puck.graph.transformations._
import puck.javaAG.nodeKind.{MethodTypeHolder, NamedTypeHolder}

import scalaz._
import Scalaz._

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

  implicit def typeCord : CordBuilder[Type[_]] = (dg, th) => th match {
    case NamedType(nid) => dg.getNode(nid).name
    case Tuple(types) =>
      types.map(t => typeCord(dg,t.asInstanceOf[Type[_]]).toString).mkString("(", ", ", ")")
      //types.map(typeCord(dg,_)).fold
    case Arrow(in, out) =>Cord( typeCord(dg, in.asInstanceOf[Type[_]]), " -> ", typeCord(dg, out.asInstanceOf[Type[_]]))
  }

  implicit def typeHolderCord : CordBuilder[TypeHolder] = (dg, th) => th match {
    case NoType => ""
    case NamedTypeHolder(t) => Cord(" : ", typeCord(dg, t))
    case MethodTypeHolder(t) => Cord(" : ", typeCord(dg, t))
  }

  implicit def nodeIdCord : CordBuilder[NodeId] = { (dg, nid) =>
    val n = dg.getNode(nid)
    Cord(s"${n.id} - ${n.kind} ${n.name}", typeHolderCord(dg, n.styp))
  }

  implicit def nodeCord : CordBuilder[DGNode] = (dg, n) => Cord(s"${n.id} - ${n.kind} ${n.name}" , typeHolderCord(dg, n.styp))
  def nodeNameTypCord : CordBuilder[DGNode] = (dg, n) => Cord(n.name , typeHolderCord(dg, n.styp))
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
      case TTNode(_, _ ,_ ,_ ,_) | TTEdge(_) =>
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
