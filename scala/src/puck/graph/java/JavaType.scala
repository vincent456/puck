package puck.graph.java

import puck.graph.{AGNode, NamedType, Arrow, Type}
import puck.graph.java.JavaNodeKind.{AbstractMethod, Method, Class, Interface}

/**
 * Created by lorilan on 27/05/14.
 */


class JavaType(n : AGNode) extends NamedType(n){

  def hasMethodThantCanOverride(name : String, sig : Arrow) : Boolean =
    n.content.exists{ (childThis : AGNode) =>
      childThis.name == name &&
        (childThis.kind match {
          case Method(thist : Arrow) => thist.canOverride(sig)
          case _ => false
        })
    }

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
                case Method(t : Arrow) => hasMethodThantCanOverride(childOther.name, t)
                case AbstractMethod(t : Arrow) => hasMethodThantCanOverride(childOther.name, t)
                case _ => true
              }
            }
          case _ => false
        }
      case _ => false
    })
}

class MethodType(input:Type, output:Type) extends Arrow(input, output){
     override def canOverride(other : Arrow) : Boolean = other match {
       case Arrow(i, o) => i == input && output.subtypeOf(o)
     }
}
