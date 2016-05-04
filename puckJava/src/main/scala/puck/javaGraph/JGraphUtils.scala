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

package puck.javaGraph

import puck.graph.io.DotHelper
import puck.graph.transformations.TransformationRules
import puck.graph.{NodeKind, NodeKindKnowledge, GraphUtils}
import puck.javaGraph.nodeKind._
import puck.javaGraph.transformations.{JavaIntro, JavaAbstract, JavaRenamer, JavaTransformationHelper}

/**
  * Created by Loïc Girault on 2/21/16.
  */
abstract class JGraphUtils extends GraphUtils {

  val nodeKindKnowledge : NodeKindKnowledge = JavaNodeKind

  val Rules: TransformationRules =
    new TransformationRules(JavaTransformationHelper, JavaRenamer, JavaAbstract, JavaIntro)

  val violationsKindPriority: Seq[NodeKind] =
    Seq[JavaNodeKind]( Field, Constructor, Class, Interface)

  implicit val dotHelper : DotHelper = JavaDotHelper

}
