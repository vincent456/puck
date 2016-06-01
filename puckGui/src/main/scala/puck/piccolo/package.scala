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

package puck

import java.awt.geom.Rectangle2D

import org.piccolo2d.util.PBounds

/**
  * Created by Loïc Girault on 31/05/16.
  */
package object piccolo {
  implicit class BoundsOp(val b: PBounds) extends AnyVal {
    def copy(x: Double = b.getX, y: Double = b.getY,
             width: Double = b.getWidth,
             height: Double = b.getHeight) = new PBounds(x, y, width, height)

    def rectangle : Rectangle2D =
      new Rectangle2D.Double(b.getX, b.getY, b.getWidth, b.getHeight)
  }

  def squareSide(numChild : Int ) : Int = {

    def aux(i : Int) : Int =
      if(i * i >= numChild) i
      else aux(i + 1)

    aux(1)
  }
}
