package puck.graph.io

import puck.graph.error
import java.io.File

import scala.xml.{NodeSeq, XML, Node}
/**
  * Created by lorilan on 08/02/16.
  */
object ConfigParser {

  import puck.util.FileHelper.FileOps

  def apply(fh : Project) : Unit =
       apply(fh, fh.workingDirectory \ Project.Default.config)

  import Utils._


  def pathToFile(fh : Project, p : String) : File =
  if(p startsWith "/") new File(p)
  else fh.workingDirectory \ p

  def apply(fh : Project, config : File) : Unit = {
    val n = XML.loadFile(config)

    (n \ "src" textOption) foreach {
      sr => fh.srcDirectory set Some(pathToFile(fh, sr))
    }

    (n \ "lib" textOption) foreach {
      sr => fh.libDirectory set Some(pathToFile(fh, sr))
    }

    (n \ "out" textOption)foreach {
      sr => fh.outDirectory set Some(pathToFile(fh, sr))
    }

    (n \ "decouple" textOption)foreach {
      sr => fh.decouple set Some(pathToFile(fh, sr))
    }

    (n \ "java-rt" textOption)foreach {
      sr => fh.javaRuntime set Some(pathToFile(fh, sr))
    }
  }
}

object Utils {
  def singleOptionAttribute(node : Node, attr : String) : Option[String] =
    node attribute attr match {
      case Some(Seq(att)) => Some(att.text)
      case Some(Nil) | None => None
      case _ => error(s"one or zero attribute $attr expected in node " + node.toString())
    }

  def optionAttribute[A](node : Node, attr : String, convert : String => A) : Option[A] =
    singleOptionAttribute(node, attr) map convert

  def optionBooleanAttribute(node : Node, attr : String) : Boolean =
    optionAttribute(node,attr, _.toBoolean) getOrElse false


  def singleAttribute(node : Node, attr : String) : String =
    node attribute attr match {
      case Some(Seq(att)) => att.text
      case Some(atts) if atts.nonEmpty => error(s"only one $attr expected")
      case _ => error(s"attribute $attr expected in node " + node.toString())
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