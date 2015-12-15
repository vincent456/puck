package puck.javaGraph
package nodeKind

import puck.graph._
import puck.graph.constraints.AbstractionPolicy
import ShowDG._
import puck.graph.transformations.rules.Intro
import puck.javaGraph.transformations.JavaIntro

abstract class JavaNodeKind extends NodeKind {
  /*def packageNode : AGNode[JavaNodeKind] =
   this match {
     case Package(id) => this.node
     case _ => this.node.container.kind.packageNode
   }*/
}

case object Definition extends JavaNodeKind {
  override def canContain(k: NodeKind): Boolean = false

  override def abstractionNodeKinds(p: AbstractionPolicy): Seq[NodeKind] = Seq()

  override def kindType: KindType = ValueDef
}

case object TypeVariable extends JavaNodeKind {
  def kindType : KindType = InstanceTypeDecl
  def canContain(k : NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object WildCardType extends JavaNodeKind {
  def kindType : KindType = InstanceTypeDecl
  def canContain(k : NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}

case object Param extends JavaNodeKind {

  def canContain(k: NodeKind): Boolean = false
  def abstractionNodeKinds(p: AbstractionPolicy): Seq[NodeKind] = Seq()
  def kindType: KindType = Parameter
}

object JavaNodeKind extends NodeKindKnowledge {

  def rootKind : NodeKind = JavaRoot

  def lightKind : NodeKind = Interface


  def kindOfKindType(kindType: KindType) : Seq[NodeKind] =
    kindType match {
      case NameSpace => Seq(Package)
      case TypeConstructor => Seq(Constructor)
      case TypeDecl => Seq(Interface, Class)
      case InstanceValueDecl => Seq(Field, Method)
      case InstanceTypeDecl => Seq(Interface, Class)
      case StaticValueDecl => Seq(StaticField, StaticMethod)
      case Parameter => Seq(Param)
      case ValueDef => Seq(Definition)
      case UnknownKindType => sys.error("Unknown kind type")
    }



  val nodeKinds = List[NodeKind](JavaRoot, Package, Interface,
    Class, Constructor, Method, /*ConstructorMethod,*/
    Field, AbstractMethod, Literal, Primitive)

  def concreteNodeTestPred(graph : DependencyGraph, nid : NodeId)
                          (pred: ConcreteNode => Boolean): Boolean =
    graph.getNode(nid) mapConcrete (pred, false)

  override def canContain
  ( graph : DependencyGraph,
    n : DGNode,
    other : ConcreteNode) : Boolean = {
    val id = n.id

    def noNameClash( l : Int )( cId : NodeId ) : Boolean =
      concreteNodeTestPred(graph, cId){ c =>
        (c.kind, graph.structuredType(c.id)) match {
          case (ck: MethodKind, Some(Arrow(Tuple(input), _)))=>
            c.name != other.name || input.length != l
          case (ck: MethodKind, _)=>
            throw new DGError((graph, c).shows)
          case _ => true
        }
      }

    def implementMethod
    ( absMethodName : String,
      absMethodType : Arrow)(id : NodeId) : Boolean =
      graph.content(id).exists(concreteNodeTestPred(graph, _) { c =>
        (c.kind, graph.structuredType(c.id)) match {
          case (Method, Some(mt @ Arrow(_, _))) =>
            absMethodName == c.name && absMethodType == mt
          case (Method, _) => throw new DGError()
          case _ => false
        }
      })

    super.canContain(graph, n, other) &&
      (!other.kind.isInstanceOf[MethodKind] || {
      (other.kind, graph.structuredType(other.id)) match {
        case (AbstractMethod, Some(absMethodType @ Arrow(Tuple(input), _))) =>
          graph.content(id).forall(noNameClash(input.length)) &&
            graph.directSubTypes(id).forall {implementMethod(other.name, absMethodType)}

        /* cannot have two methods with same name and same type */
        case (Method | StaticMethod, Some(Arrow(Tuple(input), _))) =>
          graph.content(id).forall(noNameClash(input.length))


        case (_ : MethodKind, st) =>
          throw new DGError(s"canContain(${(graph, id).shows}, ${(graph, other.id).shows}) $st")

        case d => error(d + " should not happen")
      }
    })


  }

  def defaultKindForNewReceiver : NodeKind = Field

  val initializerKind : NodeKind = Method

  val intro : Intro = JavaIntro

  def getConstructorOfType(g: DependencyGraph, tid : NodeId) : Option[NodeId] = {
    g.content(tid).find {
      cid =>
        val n = g.getConcreteNode(cid)
        n.kind match {
          case Constructor => g.parameters(cid).isEmpty
          case _ => false
        }
    }
  }

  override def writeType(graph: DependencyGraph): Type = {
    val sNode = graph.concreteNodes.find(_.name == "void")
    if(sNode.isEmpty) error("void not loaded")
    else NamedType(sNode.get.id)
  }

  override def structuredType(graph : DependencyGraph, id : NodeId, params : List[NodeId]) : Option[Type] =
    //assert node is a typed value
    if(params.nonEmpty) super.structuredType(graph, id, params)
    else {
      val n = graph.getNode(id)
      (n.kind, graph styp id) match {
      case (_ : MethodKind, Some(t)) =>Some(Arrow(Tuple(), t))
      case (_, Some(t))=> Some(t)
      case (_, None) => None /*error(s"missing type for $n")*/

    }
  }
}