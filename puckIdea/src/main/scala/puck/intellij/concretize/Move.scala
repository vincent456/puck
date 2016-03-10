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

package puck.intellij.concretize

import com.intellij.psi.PsiJavaFile
import puck.graph.{DependencyGraph, NodeId, DGEdge}
import puck.graph.transformations._
import puck.intellij.{TypedKindDeclHolder, PackageDummyWrapper, PsiNodeWrapper}

/**
 * Created by Loïc Girault on 04/11/15.
 */
object Move {

  def unapply(t : Transformation) : Option[(DGEdge, NodeId)] =
    t match {
      case Transformation(_, RedirectionOp(e, Source(newSource))) =>
        Some((e, newSource))
      case _ => None
    }



  def apply
  ( reenactor : DependencyGraph,
    id2declMap: Map[NodeId, PsiNodeWrapper],
    e : DGEdge,
    n : NodeId) : Unit = {
    val container = reenactor getConcreteNode e.container
    val contained  = reenactor getConcreteNode e.content
    val newContainer = reenactor getConcreteNode n

    assert (container.kind == newContainer.kind)

    (id2declMap(e.container), id2declMap(e.content)) match {
      case (PackageDummyWrapper, t : TypedKindDeclHolder)=>
        val c = t.node
        val f = c.getContainingFile.asInstanceOf[PsiJavaFile]
        //todo check if return also innerclasses
        if(f.getClasses.length==1){
          f.setPackageName(newContainer.name)
          //f.getVirtualFile.move()
        }

    }


  }
}
