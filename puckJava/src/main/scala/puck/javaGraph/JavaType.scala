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



//class JavaNamedType(n : NodeId) extends NamedType(n){
//
// override def copy(n : NodeId = n) = new JavaNamedType(n)

 /*def hasMethodThatCanOverride(name : String, sig : MethodType) : Boolean =
    n.content.exists{ (childThis : AGNode[JavaNodeKind]) =>
      childThis.name == name &&
        (childThis.kind match {
          case m @ Method() => m.`type`.canOverride(sig)
          case _ => false
        })
    }*/
/*
  // compute structural subtyping in addition to registered named subtyping
  override def subtypeOf(other : Type) : Boolean = super.subtypeOf(other) ||
    ( other match {
      case NamedType(othern) =>
        (n.kind, othern.kind) match {
          case (Interface(), Interface())
            | (Class(), Interface())
            | (Class(), Class()) =>
            /* TODO Find what to do about fields
               for now let's consider only the methods */
            othern.content forall { (childOther : AGNode) =>
                childOther.kind match {
                case m @ Method() => hasMethodThantCanOverride(childOther.name, m.`type`)
                case am @ AbstractMethod() => hasMethodThantCanOverride(childOther.name, am.`type`)
                case _ => true
              }
            }
          case _ => false
        }
      case _ => false
    })*/
//}

//object MethodType{
// def unapply( mt : MethodType) : Some[(Tuple, NamedType)] =
//   Some((mt.input, mt.output))
//
//  def apply(input : Tuple, output : NamedType) =
//   new MethodType(input, output)
//
//  def fromArrow( a : Arrow) = a.uncurry match {
//    case Arrow(t : Tuple, n : NamedType) => MethodType(t,n)
//    case _ => throw PuckError("cannot convert arrow into method")
//  }
//
//}
//
//class MethodType(override val input : Tuple,
//                 override val output : NamedType)
//  extends Arrow(input, output) {
//
//  /*override def equals(other : Any) = other match {
//    case that : MethodType => that.canEqual(this) &&
//      that.input == this.input && that.output == this.output
//    case _ => false
//  }
//
//  def canEqual( that : MethodType ) = true
//
//  override def hashCode = 41 * input.hashCode + output.hashCode() + 41*/
//  override def toString = "MethodType(" + input +" -> " + output +")"
//
//  import puck.util.Collections.SelectList
//  override def removeFirstArgOfType(n : Type) : MethodType =
//    input.types.select(_ == n) match {
//     case Some((_, ts)) => MethodType(Tuple(ts), output)
//     case None => this
//    }
//
//  override def  prependParameter(t : Type) : Arrow =
//    new MethodType(Tuple(t :: input.types), output)
//
//  override def canOverride(graph : DependencyGraph, other : Type) : Boolean =
//    other match {
//      case om : MethodType => om.input == input &&
//        output.subtypeOf(graph, om.output)
//      case _ => false
//    }
//
//  override def changeNamedType(oldUsee : NodeId, newUsee: NodeId) : MethodType =
//    copy(input.changeNamedType(oldUsee, newUsee),
//      output.changeNamedType(oldUsee, newUsee))
//
//  override def changeNamedTypeContravariant(oldUsee : NodeId, newUsee: NodeId) =
//    copy(input.changeNamedType(oldUsee, newUsee), output)
//
//  def copy(i : Tuple, o : NamedType) : MethodType = new MethodType(i, o)
//
//  override def copy(i : Type = input, o : Type = output) : MethodType =
//    (i,o) match {
//      case (t @ Tuple(_), nt @ NamedType(_)) => copy(t, nt)
//      case _ => throw new PuckError("Trying to create ad malformed method type")
//    }
//
//}
