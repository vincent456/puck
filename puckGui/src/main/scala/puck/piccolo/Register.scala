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

package puck.piccolo

import java.beans.{PropertyChangeEvent, PropertyChangeListener}

import org.piccolo2d.PLayer
import puck.graph.{_}
import puck.gui.PuckControl

import scala.collection.mutable

object Register {



}

/**
  * Created by Loïc Girault on 31/05/16.
  */
class Register(control : PuckControl, edgeLayer : PLayer) {
  val visibleContent = new mutable.HashMap[NodeId, DGExpandableNode]()
  val invisibleContent = new mutable.HashMap[NodeId, DGExpandableNode]()

  val usesMap = new scala.collection.mutable.HashMap[NodeIdP, PUses]

  def +=(kv : (NodeId, DGExpandableNode)) = {
    println(s"${kv._1} is visible")
    invisibleContent -= kv._1
    visibleContent += kv
  }

  def -=(kv : (NodeId, DGExpandableNode)) = {
    println(s"${kv._1} is not visible")
    invisibleContent += kv
    visibleContent -= kv._1
  }

  import control.graph


  def firstVisible(nid : NodeId) : DGExpandableNode =
    visibleContent get nid match {
      case Some(n) => n
      case None => firstVisible(graph container_! nid)
    }

  def get(nid : NodeId) : Option[DGExpandableNode] =
    invisibleContent get nid match {
      case s @ Some(_) => s
      case None => visibleContent get nid
    }

  def getOrElse(nid : NodeId, t : => DGExpandableNode) : DGExpandableNode =
    invisibleContent.getOrElse(nid,
      visibleContent.getOrElse(nid, t))


  val  parentPropertyListener =
    new PropertyChangeListener() {
    def propertyChange(evt: PropertyChangeEvent): Unit = {
      val src = evt.getSource.asInstanceOf[DGExpandableNode]
      if(evt.getNewValue == null)
        Register.this -= (src.id -> src)
      else
        Register.this += (src.id -> src)
    }
  }

  val  visibilityPropertyListener =
    new PropertyChangeListener() {
      def propertyChange(evt: PropertyChangeEvent): Unit = {
        val src = evt.getSource.asInstanceOf[DGExpandableNode]
        val isVisible = src.getVisible
        if(!isVisible)
          Register.this -= (src.id -> src)
        else
          Register.this += (src.id -> src)

      }
    }

  val  usesDeletePropertyListener =
    new PropertyChangeListener() {
      def propertyChange(evt: PropertyChangeEvent): Unit = {
          val uses = evt.getSource.asInstanceOf[PUses]
          usesMap -= (uses.source.id -> uses.target.id)
          addUses(uses.usesSet)
      }
    }

  def addUses(uses : Iterable[NodeIdP]) : Unit =
  uses map {case u @ (user,used) =>
    ((firstVisible(user), firstVisible(used)), u)
  } groupBy( _._1) foreach {
    case ((visiblePUser, visiblePUsed), it) =>
      val puse =
        usesMap.getOrElse((visiblePUser.id, visiblePUsed.id), {
          val p = PUses(visiblePUser, visiblePUsed, edgeLayer)
          p.addPropertyChangeListener(PUses.property_uses_delete, usesDeletePropertyListener)
          usesMap += ((visiblePUser.id, visiblePUsed.id) -> p)
          p
      }
      )

      val groupedUses = (it map (_._2)).toList
      println((visiblePUser.id -> visiblePUsed.id)+".usesSet ++= "+ groupedUses)
      puse.usesSet ++= groupedUses
  }


}
