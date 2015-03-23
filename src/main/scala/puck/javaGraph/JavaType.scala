package puck.javaGraph

import puck.PuckError
import puck.graph._

/**
 * Created by lorilan on 27/05/14.
 */


class JavaNamedType(n : NodeId) extends NamedType(n){

 override def create(n : NodeId) = new JavaNamedType(n)

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
}

object MethodType{
 def unapply( mt : MethodType) : Option[(Tuple, NamedType)] =
   Some((mt.input, mt.output))

  /*def unapply( t : Type) : Option[(Tuple, NamedType)] =
    t match {
      case mt : MethodType => Some((mt.input, mt.output))
      case _ => None
    }*/
}

class MethodType(override val input : Tuple,
                 override val output : NamedType)
  extends Arrow(input, output){

  /*override def equals(other : Any) = other match {
    case that : MethodType => that.canEqual(this) &&
      that.input == this.input && that.output == this.output
    case _ => false
  }

  def canEqual( that : MethodType ) = true

  override def hashCode = 41 * input.hashCode + output.hashCode() + 41*/
  override def toString = "MethodType(" + input +" -> " + output +")"

  def create(i : Tuple, o : NamedType) = new MethodType(i, o)
  override def copy() = create(input.copy(), output.copy())

  override def canOverride(graph : DependencyGraph, other : Type) : Boolean =
    other match {
      case om : MethodType => om.input == input &&
        output.subtypeOf(graph, om.output)
      case _ => false
    }

  override def redirectUses(oldUsee : NIdT, newUsee: DGNode) : MethodType =
    create(input.redirectUses(oldUsee, newUsee),
      output.redirectUses(oldUsee, newUsee))

  override def redirectContravariantUses(oldUsee : NIdT, newUsee: DGNode) =
    create(input.redirectUses(oldUsee, newUsee), output)

  override def create(i : Type, o : Type) =
    (i,o) match {
      case (t @ Tuple(_), nt @ NamedType(_)) => new MethodType(t, nt)
      case _ => throw new PuckError("Trying to create ad malformed method type")
    }


  def createReturnAccess(graph : DependencyGraph,
                         id2Decl : Map[NodeId, ASTNodeLink]) =
    id2Decl(output.id) match {
    case tk : TypedKindDeclHolder => tk.decl.createLockedAccess()
    case _ => throw new JavaAGError("need a typekind as output node")
  }

  def createASTParamList(graph : DependencyGraph,
                         id2Decl : Map[NodeId, ASTNodeLink]) : Seq[AST.ParameterDeclaration] = {
    input.types.map {
      case ty : NamedType =>

        val node = graph.getConcreteNode(ty.id)
        id2Decl(ty.id) match {
          case tk : TypedKindDeclHolder =>

            new AST.ParameterDeclaration(new AST.Modifiers, tk.decl.createLockedAccess(), node.name.toLowerCase)

          case _ => throw new JavaAGError("need type kind for param list !")
        }
      case _ => throw new PuckError("Unexpected not NamedType in param list")

    }
  }
}
