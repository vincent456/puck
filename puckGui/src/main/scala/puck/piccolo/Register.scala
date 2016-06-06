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

import puck.graph.{DependencyGraph, _}

import scala.collection.mutable

/**
  * Created by Loïc Girault on 31/05/16.
  */
class Register {
  val visibleContent = new mutable.HashMap[NodeId, DGPNode]()
  val invisibleContent = new mutable.HashMap[NodeId, DGPNode]()

  def +=(kv : (NodeId, DGPNode)) = {
    invisibleContent -= kv._1
    visibleContent += kv
  }

//  def -=(k : (NodeId, DGPNode)) = {
//    invisibleContent += k
//    visibleContent -= k._1
//
//  }
//  def -=(k : NodeId) = {
//    visibleContent get k foreach {
//      v => invisibleContent += (k -> v)
//    }
//    visibleContent -= k
//  }

  def firstVisible(nid : NodeId, g : DependencyGraph) : DGPNode =
    visibleContent get nid match {
      case Some(n) => n
      case None => firstVisible(g container_! nid, g)
    }

  def get(nid : NodeId) : Option[DGPNode] =
    invisibleContent get nid match {
      case s @ Some(_) => s
      case None => visibleContent get nid
    }

  def getOrElse(nid : NodeId, t : => DGPNode) : DGPNode =
    invisibleContent.getOrElse(nid,
      visibleContent.getOrElse(nid, t))


  val  parentPropertyListener =
    new PropertyChangeListener() {
    def propertyChange(evt: PropertyChangeEvent): Unit = {
      val src = evt.getSource.asInstanceOf[DGPNode]
      if(evt.getNewValue == null) {
        //Register.this -= (src.id -> src)
        invisibleContent += (src.id -> src)
        visibleContent -= src.id
      }
      else
        Register.this += (src.id -> src)

    }
  }



}
