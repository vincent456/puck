package puck.graph

import puck.graph.transformations._

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

  def showTypeHolder(g : DependencyGraph) : Show[TypeHolder] = new Show[TypeHolder] {
    override def shows(th : TypeHolder) : String = th.mkString(g)
  }

  implicit def graphToOptionGraph(g : DependencyGraph) : Option[DependencyGraph] = Some(g)
  /*def showNode( sg : Option[DependencyGraph] = None): Show[TypeHolder] = sg match {
    case None => Show.showFromToString
    case Some(g) => new Show[DGNode] {
      override def show(n : DGNode ) = Cord()
    }
  }*/

  type CordBuilder[A] = (DependencyGraph, A) => Cord


  def showFromGraphOption[A](sg : Option[DependencyGraph] = None)
                            (s : CordBuilder[A] ) : Show[A] = sg match {
    case None => Show.showFromToString
    case Some(g) => new Show[A] {
      override def show(a : A) = s(g, a)
    }
  }

  import Cord.stringToCord


  def edgeCord : CordBuilder[DGEdge] =  (dg, e) => e.mkString(dg)
  def extremityCord : CordBuilder[Extremity] =
    (dg, e) => Cord(e.productPrefix, "(", dg.getNode(e.node).toString(dg), ")")

  def showEdge(sg : Option[DependencyGraph] = None) =
    showFromGraphOption[DGEdge](sg)(edgeCord)

  def showExty(sg : Option[DependencyGraph] = None) =
    showFromGraphOption[Extremity](sg)(extremityCord)

  def transfoTargetCord : CordBuilder[TransformationTarget] = (dg, tgt) =>
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

  def showTransfoTarget(sg : Option[DependencyGraph] = None) =
    showFromGraphOption[TransformationTarget](sg)(transfoTargetCord)

  def showTransformation(sg : Option[DependencyGraph] = None) =
    showFromGraphOption[Transformation](sg) { (dg, tf) =>
      tf.target match {
        case TTNode(_, _ ,_ ,_ ,_) | TTEdge(_) =>
          Cord(tf.operation.productPrefix, "(", transfoTargetCord(dg, tf.target),")")
        case _ =>  transfoTargetCord(dg, tf.target)
      }
    }
}
