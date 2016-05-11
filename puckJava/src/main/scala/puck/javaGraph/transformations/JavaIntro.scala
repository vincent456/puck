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

package puck.javaGraph.transformations

import puck.graph._
import puck.graph.transformations.rules.Intro
import puck.javaGraph.nodeKind._

object JavaIntro extends Intro {
  intro =>
  override def apply
  (graph: DependencyGraph,
   localName: String,
   kind: NodeKind,
   mutable: Mutability = true
    ): (ConcreteNode, DependencyGraph) = {
    val (n, g) = super.apply(graph, localName, kind, mutable)
    kind match {
      case Class =>
        val (ctorDecl, _, g1) =
          intro.typedNodeWithDef(g,localName, Constructor, n.id)

        (n, g1.addContains(n.id, ctorDecl.id))

      case _ => (n, g)
    }
  }

  def nodeWithDef
  (graph: DependencyGraph,
   localName: String,
   kind: NodeKind,
   typ: Type,
   mutable: Mutability = true
    ): (ConcreteNode, ConcreteNode, DependencyGraph) = {

    val (cn, g) = this.apply(graph, localName, kind)
    val (defNode, g2) = g.addConcreteNode(DependencyGraph.definitionName, Definition)

    (cn, defNode,
      g2.addType(cn.id, typ)
      .addEdge(Contains(cn.id, defNode.id)))
  }


  def defaultConstructor
  ( g : DependencyGraph,
    typeNode : NodeId) : LoggedTry[(ConcreteNode, DependencyGraph)] = {

    val (cn, _, g2)=
      intro.nodeWithDef(g,
        g.getConcreteNode(typeNode).name, Constructor,
        NamedType(typeNode), mutable = true)
    LoggedSuccess((cn, g2.addContains(typeNode, cn.id)))
  }
}
