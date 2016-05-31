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

import java.awt.geom.Rectangle2D

import org.piccolo2d.PNode
import org.piccolo2d.nodes.PPath

object TitledGridSquareNode {
  def getSide(numChild : Int ) : Int = {

    def aux(i : Int) : Int =
      if(i * i >= numChild) i
      else aux(i + 1)

    aux(1)
  }
}

/**
  * Created by Loïc Girault on 23/05/16.
  */
class TitledGridSquareNode
( titlePnode : PNode,
  s : Int
) extends PPath.Float(new Rectangle2D.Float(0, 0, 100, 100)) {

  titlePnode.setBounds(0, 0, 100, 10)


  super.addChild(titlePnode)

  val body = new PNode() with GridLayoutPNode {
    val side = s
    setBounds(0, 0, 80, 80)
  }

  super.addChild(body)
  titlePnode.offset(0d,0d)
  body.offset(10d, 10d)

  override def addChild( child : PNode) : Unit = {
    body addChild child
    //child.scale( child.getScale * 0.9)
  }


}
