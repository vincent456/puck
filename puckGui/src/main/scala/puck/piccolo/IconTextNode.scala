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

import java.awt.{Font, Image}

import org.piccolo2d.PNode
import org.piccolo2d.nodes.{PImage, PText}
import puck.graph.{DependencyGraph, NodeId}
import puck.gui.NodeKindIcons

/**
  * Created by Loïc Girault on 26/05/16.
  */
object IconTextNode {
  def apply(g : DependencyGraph, nid : NodeId)
           (implicit icons : NodeKindIcons): IconTextNode = {
    val n = g.getNode(nid)
    import puck.graph.ShowDG._
    new IconTextNode((g, n).shows(desambiguatedLocalName),
      icons.iconOfKind(n.kind).getImage)
  }
}
class IconTextNode
(text : String,
 icon : Image )
  extends PNode {

  val picon = new PImage(icon)
  addChild(picon)
  val ptext = new PText(text) {
    setFont(new Font("SansSerif", Font.PLAIN, 8))
  }
  addChild(ptext)

  override def layoutChildren() : Unit = {
    val (x,y) = (0d, 0d)
    picon.setOffset(x, y)
    ptext.setOffset(picon.getWidth, y)
  }
}
