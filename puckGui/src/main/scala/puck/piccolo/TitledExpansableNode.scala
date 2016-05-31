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

import java.awt.{Color, Graphics2D, Paint}

import org.piccolo2d.PNode
import org.piccolo2d.util.{PBounds, PPaintContext}
import puck.graph.NodeId

/**
  * Created by Loïc Girault on 31/05/16.
  */
class TitledExpansableNode
( val id: NodeId,
  titlePnode : PNode
) extends DecoratorGroup with DGPNode{


  super.addChild(titlePnode)

  val body = new DecoratorGroup{}

  super.addChild(body)
  titlePnode.offset(0d,0d)
  body.offset(10d, 10d)

  override def addChild( child : PNode) : Unit = {
    body addChild child
    //child.scale( child.getScale * 0.9)
  }
  def contentSize: Int = body.getChildrenCount

  def clearContent(): Unit = body.removeAllChildren()
}
// cf group example
trait DecoratorGroup extends PNode  {

  private val INDENT: Int = 10
  private val cachedChildBounds: PBounds = new PBounds
  private var comparisonBounds: PBounds = new PBounds

  override def paint(ppc: PPaintContext) {
    val paint: Paint = Color.black
    if (paint != null) {
      val g2: Graphics2D = ppc.getGraphics
      g2.setPaint(paint)
      val bounds: PBounds = getUnionOfChildrenBounds(null)
      bounds.setRect(bounds.getX - INDENT, bounds.getY - INDENT, bounds.getWidth + 2 * INDENT, bounds.getHeight + 2 * INDENT)

      g2.draw(bounds.rectangle)
      //g2.fill(bounds)

    }
  }

  override def computeFullBounds(dstBounds: PBounds): PBounds = {
    val result: PBounds = getUnionOfChildrenBounds(dstBounds)
    cachedChildBounds.setRect(result)
    result.setRect(result.getX - INDENT, result.getY - INDENT, result.getWidth + 2 * INDENT, result.getHeight + 2 * INDENT)
    localToParent(result)
     result
  }

  override def validateFullBounds: Boolean = {
    comparisonBounds = getUnionOfChildrenBounds(comparisonBounds)
    if (cachedChildBounds != comparisonBounds)
      setPaintInvalid(true)

    super.validateFullBounds
  }
}