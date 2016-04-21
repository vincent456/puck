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

package puck.graph

/**
  * Created by Loïc Girault on 15/04/16.
  */
sealed abstract class TypeUseConstraint {
  def constrainedType : NodeId = constrainedUse._2
  def constrainedUse : NodeIdP
  def copyWithNewTypeUsed(tUsed : NodeId) : TypeUseConstraint
}

case class Sup(constrainedUse : NodeIdP) extends TypeUseConstraint{
  def copyWithNewTypeUsed(tUsed : NodeId) : Sup =
    copy((constrainedUse.user, tUsed))
}
case class Sub(constrainedUse : NodeIdP) extends TypeUseConstraint{
  def copyWithNewTypeUsed(tUsed : NodeId) : Sub =
    copy((constrainedUse.user, tUsed))
}
case class Eq(constrainedUse : NodeIdP) extends TypeUseConstraint{
  def copyWithNewTypeUsed(tUsed : NodeId) : Eq =
    copy((constrainedUse.user, tUsed))
}
