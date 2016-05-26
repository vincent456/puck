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

import javax.swing.ImageIcon

import puck.graph.{AGRoot, NodeKind}
import puck.gui.NodeKindIcons
import puck.javaGraph.nodeKind._

/**
  * Created by Loïc Girault on 05/01/16.
  */
object JavaIcons extends NodeKindIcons {

  val path = "/icons"

  val classIcon : ImageIcon = new ImageIcon(getClass.getResource(s"$path/class.gif"))
  val interfaceIcon : ImageIcon = new ImageIcon(getClass.getResource(s"$path/interface.gif"))

  val rootIcon : ImageIcon = new ImageIcon(getClass.getResource(s"$path/root.gif"))

  val constructorIcon : ImageIcon = new ImageIcon(getClass.getResource(s"$path/constructor.png"))

  val methodIcon : ImageIcon = new ImageIcon(getClass.getResource(s"$path/method.png"))
  val abstractMethodIcon : ImageIcon = new ImageIcon(getClass.getResource(s"$path/method_abstract.png"))
  val staticMethodIcon : ImageIcon = new ImageIcon(getClass.getResource(s"$path/method_static.png"))

  val packageIcon : ImageIcon = new ImageIcon(getClass.getResource(s"$path/package.gif"))
  val fieldIcon : ImageIcon = new ImageIcon(getClass.getResource(s"$path/field.png"))
  val staticFieldIcon : ImageIcon = new ImageIcon(getClass.getResource(s"$path/field_static.png"))

  val unknownIcon : ImageIcon = new ImageIcon(getClass.getResource(s"$path/question-mark.gif"))

  def iconOfKind(k: NodeKind): ImageIcon = k match {
    case _ : TypeKind.InterfaceLike => interfaceIcon
    case _ : TypeKind.ClassLike => classIcon
//    case InnerClass => innerClassIcon
//    case InnerInterface => innerInterfaceIcon
    case AbstractMethod => abstractMethodIcon
    case StaticMethod => staticMethodIcon
    case Method => methodIcon
    case Field => fieldIcon
    case StaticField => staticFieldIcon
    case Constructor => constructorIcon
    case _ : AGRoot => rootIcon
    case Package => packageIcon
    case _ => unknownIcon
  }
}
