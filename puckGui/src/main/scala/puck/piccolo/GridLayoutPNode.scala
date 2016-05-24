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

import java.util

import org.piccolo2d.PNode

/**
  * Created by Loïc Girault on 23/05/16.
  */
trait GridLayoutPNode {
  self : PNode =>

  val side : Int

  override def layoutChildren() : Unit = {
    var xOffset = 0d
    var yOffset = 0d

    import scala.collection.JavaConversions._
    val it  = getChildrenIterator.asInstanceOf[util.ListIterator[PNode]]
    it.zipWithIndex.foreach {
      case (n, i) =>
        if(i % side == 0) {
          xOffset = 0
          if( i > 0 )
            yOffset += (n.getHeight * n.getScale)
        }

        n.offset(xOffset, yOffset)
        xOffset += (n.getWidth * n.getScale)
    }
  }

}
