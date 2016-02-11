package puck.graph.io

import puck.graph.error
import java.io.{FileWriter, File}

import puck.graph.io.Project.Config
import puck.util.FileHelper

import scala.xml.{NodeSeq, XML, Node}
/**
  * Created by lorilan on 08/02/16.
  */
object ConfigParser {

  import Project.Default
  import Utils._

  def createDefault(configFile : File) : Config = {
      val f = new FileWriter(configFile)
      f write
        s"""<puck-config>
          |    <workspace>.</workspace>
          |    <!-- root of source where all .java will be searched -->
          |    <src rec=".java">${Default.srcRoot}</src>
          |    <!-- root of lib where all .jar will be searched -->
          |    <classpath rec=".jar">${Default.classpathRoot}</classpath>
          |    <out>${Default.out}</out>
          |    <decouple>${Default.decouple}</decouple>
          |    <log>${Default.log}</log>
          | </puck-config>""".stripMargin

      f.close()
      this.apply(configFile)
  }

  def apply(config : File) : Config = {
    val n = XML.loadFile(config)

    val root =
      (n \ Project.Keys.workspace).textOption getOrElse config.getParent

    val initConf = Project.emptyConf put (Project.Keys.workspace, root)


    val rootFile = new File(root)

    import puck.util.FileHelper.FileOps
    def resolvePath(p : String) : String =
      if(p startsWith "/") p
      else (rootFile \ p).getAbsolutePath

    val c1 = Project.singleValueKeys.foldLeft(initConf){
      case (c, k) =>
        (n \ k).textOption map {
          v => c put (k, v)
        } getOrElse c
    }

    Project.listValueKeys.foldLeft(c1){
      case (c, k) =>
        (n \ k).foldLeft(c) {
          (c1, knode) =>
            val prev = c1 getOrElse (k, List())
            val path = resolvePath(knode.text)
            val newVal =
              knode singleOptionAttribute "rec" match {
                case None =>  path :: prev
                case Some(suffix) =>
                  import puck.util.FileHelper.findAllFiles
                  findAllFiles(suffix, ignoredSubDir = None, prev, new File(path))
              }
            c1 put (k, newVal)

        }
    }
  }
}

object Utils {

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