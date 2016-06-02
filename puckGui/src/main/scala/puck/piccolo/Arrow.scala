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

import java.awt.Color
import java.awt.geom.{Path2D, Point2D}

import org.piccolo2d.extras.nodes.{PComposite, PLine}
import org.piccolo2d.nodes.PPath

/**
  * Created by Loïc Girault on 01/06/16.
  */

object Arrow {

  type P = (Double, Double)

  implicit class POps( val p : P) extends AnyVal {
    def x = p._1
    def y = p._2

    def orthogonal : P = (-y, x)

    def *(i : Int) : P = (x * i, y * i)

    def point : Point2D.Float =
      new Point2D.Float(p._1.toFloat,p._2.toFloat)
  }


  def triangle(p1 : P, p2 : P, p3 : P) : Path2D = {
    val path : Path2D = new Path2D.Double(Path2D.WIND_NON_ZERO, 3)
    path.moveTo(p1.x, p1.y)
    path.lineTo(p2.x, p2.y)
    path.lineTo(p3.x, p3.y)
    path.lineTo(p1.x, p1.y)
    path
  }

  def apply(src : Point2D, tgt : Point2D) : Arrow =
    new Arrow(src, tgt)

}
import Arrow._
class Arrow(source : P, target : P) extends PComposite {

  def this(src : Point2D, tgt : Point2D)  =
    this((src.getX, src.getY), (tgt.getX, tgt.getY))

  val theta : Double = 0.45
  val l : Double = 10d

  val bc_length : Double = l * Math.cos(theta)
  val l_sin_theta : Double =  l * Math.sin(theta)

  def arrowBaseHeadCoordinates : (P,P) = {
    val a = source
    val c = target

    val ac_length = Math.sqrt( (c.x - a.x) * (c.x - a.x) + (c.y - a.y) * (c.y - a.y) )

    val u = ( (c.x - a.x) / ac_length, (c.y - a.y) / ac_length)

    val ac_minus_bc = ac_length - bc_length

    //le point B est la projection de la base de la tête sur l'arc
    val b = (ac_minus_bc * u.x + a.x, ac_minus_bc * u.y + a.y)

    val v = u.orthogonal

    def d( v : P) : P =
      (l_sin_theta * v.x + b.x, l_sin_theta * v.y + b.y)

    (d(v), d(v * -1))
  }

  private val line = new PLine()
  line.addPoint(0, source.x, source.y)
  line.addPoint(1, target.x, target.y)

  val head = {
    val p1 = target
    val (p2, p3) = arrowBaseHeadCoordinates
    val h = PPath.createPolyline(Array(p1.point, p2.point, p3.point))
    h.setPaint(Color.BLACK)
    h
  }

  addChild(line)
  addChild(head)

}
