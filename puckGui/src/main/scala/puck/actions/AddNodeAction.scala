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

package puck.actions


import puck.graph.{GraphUtils, DependencyGraph, ConcreteNode, NodeKind}
import puck.gui.PushGraph

import scala.swing.{Action, Publisher}

class AddNodeAction
(bus : Publisher,
 host : ConcreteNode,
 childKind : NodeKind)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils)
extends Action(s"Add $childKind")
{

  import graphUtils.{Rules => TR}

  def apply(): Unit = {
    showInputDialog(s"New $childKind name:").foreach {
      childName =>
        val (n, g) = TR.intro(graph.mileStone, childName, childKind)
        bus publish PushGraph(g.addContains(host.id, n.id))
    }
  }
}
