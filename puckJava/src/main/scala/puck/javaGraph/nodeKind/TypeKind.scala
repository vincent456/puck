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

package puck.javaGraph.nodeKind


import puck.graph._

sealed abstract class TypeKind extends JavaNodeKind {
  def kindType: KindType = TypeDecl
}

case object Primitive extends TypeKind {
  def canContain(k: NodeKind) = false
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
  //throw new DGError("do not know how to abstract primitive kind")
}



case object Interface extends TypeKind {

  override val toString : String = "Interface"

  def canContain(k : NodeKind) : Boolean = {
    k match {
      case AbstractMethod
           | StaticMethod
           | StaticField  => true
      case _ => false
    }
  }
  override def canBe(k : NodeKind) : Boolean = {
    k match {
      case Interface | Inner (Interface) => true
      case _ => false
    }
  }

  def abstractionNodeKinds(p : AbstractionPolicy) = p match {
    case SupertypeAbstraction => Seq(Interface/*, GenericInterface*/)
    case DelegationAbstraction => Seq()//Class)//also interface ?
  }
}


case object Class extends TypeKind {

  override val toString : String = "Class"

  override def canBe(k : NodeKind) : Boolean =
    k match {
      case _ : TypeKind => true
      case _ => false
    }


  def canContain(k : NodeKind) : Boolean = {
    k match {
      case Constructor
           | Field
           | Method
           | AbstractMethod
           | StaticMethod
           | StaticField => true
      case _ => false
    }
  }


  def abstractionNodeKinds(p : AbstractionPolicy) : Seq[NodeKind] = p match {
    case SupertypeAbstraction => Seq[NodeKind](Interface, Class/*, GenericInterface, GenericClass*/)
    case DelegationAbstraction => Seq[NodeKind]()//Class)//also interface ?
  }

}

case object Enum extends TypeKind {

  override val toString : String = "Enum"
  override def canBe(k : NodeKind) : Boolean = false
  def canContain(k : NodeKind) : Boolean = false
  def abstractionNodeKinds(p : AbstractionPolicy) : Seq[NodeKind] = Seq()

}


case class Inner(t : TypeKind) extends TypeKind {
  def canContain(k: NodeKind): Boolean = t.canContain(k)
  def abstractionNodeKinds(p: AbstractionPolicy): Seq[NodeKind] = t.abstractionNodeKinds(p)

  override def kindType: KindType = InstanceTypeDecl
}

case object TypeVariable extends JavaNodeKind {
  def kindType : KindType = TypeVariableKT
  def canContain(k : NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object WildCardType extends TypeKind {
  def canContain(k : NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
