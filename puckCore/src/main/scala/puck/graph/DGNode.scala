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

sealed trait DGNode{
  val id : NodeId
  val name : String
  val kind : NodeKind
  //def styp(g : DependencyGraph) : Option[Type]
  val mutable : Boolean

  def mapConcrete[A](f : ConcreteNode => A, default : => A) : A
  def name(g : DependencyGraph) : String

  def definition(g : DependencyGraph) : Option[NodeId] = {
    kind.kindType match {
      case InstanceValueDecl
           | StaticValueDecl =>
        g definitionOf this.id
      case _ => None
    }
  }

  def hasDefinitionIn(g : DependencyGraph) : Boolean =
    (g definitionOf this.id).nonEmpty

  def definition_!(g : DependencyGraph) : NodeId =
    g definitionOf_! this.id


}

object DGNode {
  def apply(id : NodeId,
            name : String,
            kind : NodeKind,
            styp : Option[Type],
            isMutable : Boolean) : DGNode = ConcreteNode(id, name, kind, isMutable)
}

case class VirtualNode
(id : NodeId,
 potentialMatches : Set[NodeId],
 kind : NodeKind) extends DGNode {
  val mutable = true

  def mapConcrete[A](f : ConcreteNode => A, default : => A) : A = default

  private def mkNameString(stringify : NodeId => String) =
    potentialMatches map stringify mkString ("Virtual(", " \\/ ", ")")

  val name : String = mkNameString(_.toString)
  def name(g : DependencyGraph) : String =mkNameString(g.getNode(_).name)


}

case class ConcreteNode
( id : NodeId,
  name : String,
  kind : NodeKind,
  mutable : Boolean)  extends DGNode {


  def name(g : DependencyGraph) : String = name


  override def toString = {
    val non = if(mutable) ""
    else "non-"
    s"($id - $kind $name ${non}mutable)"
  }


  def mapConcrete[A](f : ConcreteNode => A, default : => A) : A = f(this)



  /*def distance(other : AGNode[Kind]) = {
    if(this == other) 0
    else if(this contains_*  other)
      other.containerPath(this).length - 1
    else if (other contains_* this)
      this.containerPath(other).length  - 1
    else {
      val thisPathToRoot = this.containerPath.reverse
      val otherPathToRoot = other.containerPath.reverse
      println(thisPathToRoot)
      println(otherPathToRoot)
      thisPathToRoot.foldLeft[Option[AGNode[Kind]]](None) {
        case (sn@Some(_), _) => sn
        case (None, n) =>
          if (otherPathToRoot contains n)
            Some(n)
          else
            None

      } match {
        case None =>
          Int.MaxValue
        case Some(commonAncestor) =>
          val count: ((Int, Boolean), AGNode[Kind]) => (Int, Boolean) = {
            case ((i, alreadyFound), n) =>
              if (alreadyFound || n == commonAncestor) (i, true)
              else (i + 1, false)
          }

          (thisPathToRoot.foldLeft((0, false))(count),
            otherPathToRoot.foldLeft((0, false))(count)) match {
            case ((i, _), (j, _)) => i + j
          }
      }
    }

  }*/




}
