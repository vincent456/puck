/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.util

import puck.error
import scala.xml.{Node, NodeSeq}

/**
  * Created by Loïc Girault on 12/02/16.
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
