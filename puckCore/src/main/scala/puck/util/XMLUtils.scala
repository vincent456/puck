package puck.util

import puck.error
import scala.xml.{Node, NodeSeq}

/**
  * Created by lorilan on 12/02/16.
  */
object XMLUtils {

  implicit class NodeOps(val node : Node) extends AnyVal {

    def singleOptionAttribute(attr : String) : Option[String] =
      node attribute attr match {
        case Some(Seq(att)) => Some(att.text)
        case Some(Nil) | None => None
        case _ => error(s"one or zero attribute $attr expected in node " + node.toString())
      }

    def optionAttribute[A](attr : String, convert : String => A) : Option[A] =
      singleOptionAttribute(attr) map convert

    def optionBooleanAttribute(node : Node, attr : String) : Boolean =
      optionAttribute(attr, _.toBoolean) getOrElse false


    def singleAttribute(node : Node, attr : String) : String =
      node attribute attr match {
        case Some(Seq(att)) => att.text
        case Some(atts) if atts.nonEmpty => error(s"only one $attr expected")
        case None => error(s"attribute $attr expected in node " + node.toString())
      }
  }


  implicit class NodeSeqOps(val ns : NodeSeq) extends AnyVal {
    def toNode : Node = nodeSeqToNode(ns)

    def singleText : String = nodeSeqToNode(ns).text

    def int : Int = singleText.toInt

    def toNodeOption : Option[Node] = {
      ns.theSeq match {
        case Seq(n) => Some(n)
        case Nil => None
        case _ => error("Should have only one or zero node :" + ns.mkString(","))
      }
    }

    def textOption : Option[String] = toNodeOption map (_.text)

  }

  implicit def nodeSeqToString( ns : NodeSeq) : String =
    NodeSeqOps(ns).singleText

  implicit def nodeSeqToNode( ns : NodeSeq) : Node = {
    ns.theSeq match {
      case Nil => error("EmptySeq should have one node")
      case Seq(n) => n
      case _ => error("Should have only one node :" + ns)
    }
  }

}
