package puck.javaGraph
package nodeKind


import puck.graph.ShowDG._
import puck.graph._
import puck.graph.constraints.AbstractionPolicy

/**
 * Created by lorilan on 31/07/14.
 */
abstract class JavaNodeKind extends NodeKind{
  /*def packageNode : AGNode[JavaNodeKind] =
   this match {
     case Package(id) => this.node
     case _ => this.node.container.kind.packageNode
   }*/
}

case object TypeVariable extends JavaNodeKind{
  def canContain(k : NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractKinds(p : AbstractionPolicy) = Seq()
}
case object WildCardType extends JavaNodeKind{
  def canContain(k : NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractKinds(p : AbstractionPolicy) = Seq()
}

object JavaNodeKind extends NodeKindKnowledge {

  //import AccessGraph.dummyId
  /*def packageKind = Package(dummyId)
  def interface = Interface(dummyId, None)*/
  def classKind = Class
  //fix for accessing the field in java
  def interfaceKind = Interface

  def field = Field
  def constructor = Constructor
  def abstractMethod = AbstractMethod
  def method = Method

  def primitive = Primitive
  def typeVariable = TypeVariable
  def wildcardType = WildCardType

  def noType = NoType

  val nodeKinds = Seq[NodeKind](Package, Interface,
    Class, Constructor, Method, /*ConstructorMethod,*/
    Field, AbstractMethod, Literal, Primitive)

  def concreteNodeTestPred(graph : DependencyGraph, nid : NodeId)
                          (pred: ConcreteNode => Boolean): Boolean =
    graph.getNode(nid) mapConcrete (pred, false)

  override def canContain(graph : DependencyGraph)
                         (n : DGNode, other : ConcreteNode) : Boolean = {
    val id = n.id

    def noNameClash( l : Int )( cId : NodeId ) : Boolean =
      concreteNodeTestPred(graph, cId){ c =>
        (c.kind, c.styp) match {
          case (ck: MethodKind, MethodTypeHolder(typ))=>
            c.name != other.name || typ.input.length != l
          case (ck: MethodKind, _)=> throw new DGError()
          case _ => true
        }
      }

    def implementMethod(absMethodName : String, absMethodType : Arrow)(id : NodeId) : Boolean =
      graph.content(id).exists(concreteNodeTestPred(graph, _) { c =>
        (c.kind, c.styp) match {
          case (Method, MethodTypeHolder(typ)) => absMethodName == c.name && absMethodType == typ
          case (Method, _) => throw new DGError()
          case _ => false
        }
      })

    super.canContain(graph)(n, other) &&
      ( (other.kind, other.styp) match {
        case (AbstractMethod, MethodTypeHolder(absMethodType)) =>
          graph.content(id).forall(noNameClash(absMethodType.input.length)) &&
            graph.directSubTypes(id).forall {implementMethod(other.name, absMethodType)}

        case (AbstractMethod, _) => throw new DGError(other + " does not have a MethodTypeHolder")
        /* cannot have two methods with same name and same type */
        case (Method, MethodTypeHolder(typ)) =>
          graph.content(id).forall(noNameClash(typ.input.length))
        case (Method, _) => throw new DGError(s"canContain(${showDG[NodeId](graph).shows(id)}, ${showDG[NodeId](graph).shows(other.id)})")
        case _ => true
      })
  }

  def isTypeUse : DependencyGraph => DGEdge => Boolean = graph => {
    case DGEdge(Uses, _, id) =>
      concreteNodeTestPred(graph, id){ cn => cn.kind == Interface || cn.kind == Class}
    case _ => false
  }
  def isTypeMemberUse : DependencyGraph => DGEdge => Boolean = graph => {
    case DGEdge(Uses, _, id) =>
      concreteNodeTestPred(graph, id){ cn => cn.kind match {
        case _: MethodKind | Field | Constructor => true
        case _ => false
      }
      }
    case _ => false
  }

  override def coupling(graph : DependencyGraph) =
    graph.concreteNodes.foldLeft(0 : Double){ (acc, n) => n.kind match {
    case Package =>
      val c = Metrics.coupling(n.id, graph)
      if(c.isNaN) acc
      else acc + c
    case _ => acc
  }}
}

case class NamedTypeHolder(typ : NamedType) extends TypeHolder{

  def getTypeNodeIds : List[NodeId] = List(typ.id)


  def redirectUses(oldUsee : NodeId,
                   newUsee: DGNode) : TypeHolder=
  NamedTypeHolder(typ.redirectUses(oldUsee, newUsee))

  def redirectContravariantUses(oldUsee : NodeId, newUsee: DGNode) =
    redirectUses(oldUsee, newUsee)

}

case class MethodTypeHolder(typ : MethodType) extends TypeHolder{

  def getTypeNodeIds : List[NodeId] = typ.ids

  def redirectUses(oldUsee : NodeId,
                   newUsee: DGNode) : TypeHolder=
    MethodTypeHolder(typ.redirectUses(oldUsee, newUsee))
  def redirectContravariantUses(oldUsee : NodeId, newUsee: DGNode) =
    MethodTypeHolder(typ.redirectContravariantUses(oldUsee, newUsee))

}