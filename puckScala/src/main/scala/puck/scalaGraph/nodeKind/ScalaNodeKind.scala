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

package puck.scalaGraph.nodeKind

import puck.graph._
import puck.graph.constraints.AbstractionPolicy
import puck.graph.transformations.rules.Intro

trait ScalaNodeKind extends NodeKind
object ScalaNodeKind extends NodeKindKnowledge {
  override def rootKind: NodeKind = ScalaRoot

  override def kindOfKindType(kindType: KindType): Seq[NodeKind] =
    kindType match {
      case NameSpace => Seq(Package)
      case TypeConstructor => Seq(PrimaryConstructor, SecondaryConstructor)
      case TypeDecl => Seq(Trait, Class, Type, Object)
      case InstanceValueDecl => Seq(Def, Var, Val)
      case InstanceTypeDecl => Seq(Trait, Class, Object, Type, Def, Var, Val)
      case UnknownKindType => sys.error("Unknown kind type")
    }

  override def nodeKinds: List[NodeKind] = List(Package, PackageObject, Trait, Type, Object, Var, Val, Def)

  override def lightKind: NodeKind = ???

  def writeType(graph : DependencyGraph) : Type = ???

  override def defaultKindForNewReceiver: NodeKind = ???

  def initializerKind : NodeKind = ???

  override def intro: Intro = ???

  override def getConstructorOfType(g: DependencyGraph, tid: NodeId): Option[NodeId] = ???
}

case object ScalaRoot extends ScalaNodeKind with AGRoot {
  override def canContain(k: NodeKind) = k match {
    case Package => true
    case _ => false
  }
}
case object Package extends ScalaNodeKind {
  def kindType: KindType = NameSpace
  def canContain(k : NodeKind) : Boolean = {
    k match {
      case Package
        | Class
        | Trait
        | Object
        | PackageObject => true
      case _ => false
    }
  }

  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object PackageObject extends ScalaNodeKind {
  def kindType: KindType = ???
  def canContain(k : NodeKind) : Boolean = {
    k match {
      case Package
           | PackageObject => false
      case _ => true
    }
  }
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}


trait ScalaTypeDeclKind extends ScalaNodeKind {
  def canContain(k : NodeKind) : Boolean = {
    k match {
      case Package
      | PackageObject => false
      case _ => true
    }
  }
}

case object Class extends ScalaTypeDeclKind {
  def kindType: KindType = ???
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object Trait extends ScalaTypeDeclKind {
  def kindType: KindType = ???
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object Type extends ScalaNodeKind {
  def kindType: KindType = ???
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()

  override def canContain(k: NodeKind): Boolean = false
}

case object PrimaryConstructor extends ScalaNodeKind {
  def kindType: KindType = TypeConstructor
  def canContain(k : NodeKind) : Boolean = false
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object SecondaryConstructor extends ScalaNodeKind {
  def kindType: KindType = TypeConstructor

  def canContain(k : NodeKind) : Boolean = false
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}


case object Object extends ScalaTypeDeclKind {
  def kindType: KindType = ???

  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object Val extends ScalaNodeKind {
  def kindType: KindType = ???
  def canContain(k : NodeKind) : Boolean = false
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object Var extends ScalaNodeKind {
  def kindType: KindType = ???
  def canContain(k : NodeKind) : Boolean = false
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object Def extends ScalaNodeKind {
  def kindType: KindType = ???
  def canContain(k : NodeKind) : Boolean = false
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}

