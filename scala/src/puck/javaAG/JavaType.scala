package puck.javaAG

import puck.graph._
import puck.javaAG.JavaNodeKind.{AbstractMethod, Method, Class, Interface}

/**
 * Created by lorilan on 27/05/14.
 */


class JavaType(n : AGNode) extends NamedType(n){

 def hasMethodThatCanOverride(name : String, sig : MethodType) : Boolean =
    n.content.exists{ (childThis : AGNode) =>
      childThis.name == name &&
        (childThis.kind match {
          case m @ Method() => m.`type`.canOverride(sig)
          case _ => false
        })
    }
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
}

class MethodType(override val input: Tuple, output:Type) extends Arrow(input, output){
     def canOverride(other : MethodType) : Boolean =
        other.input == input && output.subtypeOf(other.output)

}
