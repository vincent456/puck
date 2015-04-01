package puck.javaGraph.transformations

import puck.PuckError
import puck.graph.ShowDG._
import puck.graph._
import puck.graph.constraints.{AbstractionPolicy, Move, RedirectionPolicy, SupertypeAbstraction}
import puck.graph.transformations.{MergeMatcher, TransformationRules}
import puck.javaGraph.MethodType
import puck.javaGraph.nodeKind._

import scalaz.Validation.FlatMap._
import scalaz._

/**
 * Created by lorilan on 25/01/15.
 */

object JavaTransformationRules extends TransformationRules {

  override def absIntroPredicate( graph : GraphT,
                                  impl : DGNode,
                                  absPolicy : AbstractionPolicy,
                                  absKind : NodeKind) : NodePredicateT = {
    (impl.kind, absPolicy) match {
      case (Method, SupertypeAbstraction)
           | (AbstractMethod, SupertypeAbstraction) =>
        (graph, potentialHost) => !graph.interloperOf(graph.container(impl.id).get, potentialHost.id)
      case _ => super.absIntroPredicate(graph, impl, absPolicy, absKind)
    }
  }

  override def abstractionName( g: GraphT, impl: ConcreteNode, abskind : NodeKind, policy : AbstractionPolicy) : String = {
    if (impl.kind == Constructor)
      "create"
    else
      (abskind, policy) match {
        case (Method, SupertypeAbstraction)
             | (AbstractMethod, SupertypeAbstraction) => impl.name
        case _ => super.abstractionName(g, impl, abskind, policy)

      }
  }

  def insertInTypeHierarchy(g : GraphT, classId : NodeId, interfaceId : NodeId) : GraphT =
    g.directSuperTypes(classId).foldLeft(g){ (g0, superType) =>
      g0.changeSource(DGEdge.isa(classId, superType), interfaceId)
    }

  def addTypesUses(g : GraphT, node : ConcreteNode) : GraphT =
    node.styp.map(_.ids) match {
      case None => g
      case Some(typesUsed) =>
        typesUsed.foldLeft(g){(g0, tid) => g0.addUses(node.id, tid)}
    }

  def createAbstractMethod(g : GraphT, meth : ConcreteNode,
                           clazz : ConcreteNode, interface : ConcreteNode) : Try[GraphT] ={
    def addContainsAndRedirectSelfType
    (g: GraphT, methodNode: ConcreteNode): Try[GraphT] = {
      if(methodNode.kind != AbstractMethod)
        Failure(new DGError(s"$methodNode should be an abstract method !")).toValidationNel
      else Success {
        g.addContains(interface.id, methodNode.id)
          //TODO check why it is not needed
          //addTypesUses(g4, absChild)
          .changeType(methodNode.id, methodNode.styp, clazz.id, interface.id)}
    }

    createAbstraction(g, meth, AbstractMethod,  SupertypeAbstraction) flatMap {
      case (absMethod, g21) => addContainsAndRedirectSelfType(g21, absMethod)
    }
  }

  def changeSelfTypeBySuperInMethodSignature(g : GraphT, meth : ConcreteNode,
                                             clazz : ConcreteNode, interface : ConcreteNode): Try[GraphT] ={

    val g1 = g.changeContravariantType(meth.id, meth.styp, clazz.id, interface.id)

    if(g1.uses(meth.id, clazz.id)) {
      g.logger.writeln(s"interface creation : redirecting ${DGEdge.uses(meth.id, clazz.id)} target to $interface")
      redirectUsesAndPropagate(g1, DGEdge.uses(meth.id, clazz.id), interface.id, SupertypeAbstraction)
    }
    else Success(g1)
  }


  def membersToPutInInterface(g : GraphT, clazz : ConcreteNode) : Seq[ConcreteNode] = {

    def canBeAbstracted(member : ConcreteNode) : Boolean = {
      //the originSibling arg is needed in case of cyclic uses
      def aux(originSibling : ConcreteNode)(member: ConcreteNode): Boolean = {

        def sibling: NodeId => Boolean =
          sid => g.contains(clazz.id, sid) && sid != originSibling.id

        member.kind match {
          case ck: MethodKind =>
            val usedNodes = g.usedBy(member.id)
            usedNodes.isEmpty || {
              val usedSiblings = usedNodes filter sibling
              usedSiblings.map(g.getConcreteNode).forall {
                used0 => aux(member)(used0) || {
                  val typeUses = g.typeUsesOf((member.id, used0.id))
                  typeUses.forall { DGEdge.uses(_).selfUse }
                }
              }
            }
          case _ => false
        }
      }
      aux(member)(member)
    }

    g.content(clazz.id).foldLeft(Seq[ConcreteNode]()){
        (acc, mid) =>
          val member = g.getConcreteNode(mid)
          if(canBeAbstracted(member)) member +: acc
          else acc
      }
  }

  def createInterfaceAndReplaceBySuperWherePossible(g : GraphT, clazz : ConcreteNode) : Try[(ConcreteNode, GraphT)] = {
    val classMembers = g.content(clazz.id)

    for{
       itcGraph <- super.createAbstraction(g, clazz, Interface, SupertypeAbstraction).map {
            case (itc, g0) => (itc, insertInTypeHierarchy(g0, clazz.id, itc.id))
       }

       (interface, g1) = itcGraph
       members = membersToPutInInterface(g1, clazz)
       g2 <- traverse(members, g1){ (g0, member) =>
         createAbstractMethod(g0, member, clazz, interface)
       }

       g3 <- traverse(members, g2.addIsa(clazz.id, interface.id)){ (g0, child) =>
           (child.kind, child.styp) match {
              // even fields can need to be promoted if they are written
              //case Field() =>
              case (ck : MethodKind, Some(MethodType(_, _)))  =>
                changeSelfTypeBySuperInMethodSignature(g0, child, clazz, interface)
              case _ => Success(g0)
           }
       }
    } yield {
      logInterfaceCreation(g3, interface)
      (interface, g3)
    }
  }

  def logInterfaceCreation(g : GraphT, itc : ConcreteNode) : Unit = {
    g.logger.writeln(s"interface $itc created, contains : {")
    g.logger.writeln(g.content(itc.id).map(showDG[NodeId](g).show).mkString("\n"))
    g.logger.writeln("}")
  }

  override def createAbstraction(g : GraphT,
                                 impl: ConcreteNode,
                                 abskind : NodeKind ,
                                 policy : AbstractionPolicy) : Try[(ConcreteNode, GraphT)] = {

    (abskind, policy) match {
      case (Interface, SupertypeAbstraction) =>
        createInterfaceAndReplaceBySuperWherePossible(g, impl)

      case (AbstractMethod, SupertypeAbstraction) =>
        //no (abs, impl) or (impl, abs) uses
        Success(createAbsNode(g, impl, abskind, policy))

      case (ConstructorMethod, _) =>
        super.createAbstraction(g, impl, abskind, policy) map { case (abs, g0) =>
          (abs, addTypesUses(g0, abs))
        }

      case _ => super.createAbstraction(g, impl, abskind, policy)
    }
  }

  override def abstractionCreationPostTreatment(g: GraphT,
                                                implId : NodeId,
                                                absId : NodeId,
                                                policy : AbstractionPolicy) : GraphT = {
    val abstraction = g.getNode(absId)
    (abstraction.kind, policy) match {
      case (AbstractMethod, SupertypeAbstraction) =>
        val implContainer = g.container(implId).get
        val thisClassNeedsImplement = (g.abstractions(implContainer) find
          {case (abs, absPolicy) => absPolicy == SupertypeAbstraction &&
            abs == g.container(absId).get}).isEmpty

        if(!thisClassNeedsImplement) g
        else {
          val absContainer = g.container(absId).get
          val g1 = g.addUses(implContainer, absContainer)
            .addIsa(implContainer, absContainer)

          g1.content(absId).foldLeft(g1){
            case (g0, absMethodId) => val absMeth = g0.getConcreteNode(absMethodId)
              g0.changeType(absMethodId, absMeth.styp, implId, absId)
          }
        }
      case _ => g
    }
  }



  override def redirectUsesAndPropagate(g : GraphT,
                            oldEdge : EdgeT, newUsee : NodeId,
                            policy : RedirectionPolicy,
                            propagateRedirection : Boolean = true,
                            keepOldUse : Boolean = false ) : Try[GraphT] = {

    val tryEdgeGraph =
      super.redirectUsesAndPropagate(g, oldEdge, newUsee, policy,
        propagateRedirection, keepOldUse)

    g.getConcreteNode(oldEdge.used).kind match {
      case Constructor =>
        val ctorId = oldEdge.used
        g.container(ctorId) match {
          case None => throw new PuckError("constructor should have a container")
          case Some(classId) => tryEdgeGraph map { g0 =>
             g.users(oldEdge.user).foldLeft(g0){ case (g1, userId) =>
              g1.addUses(userId, ctorId).addUses(userId, classId)}
          }
        }

      case _ => tryEdgeGraph
    }
  }

  /*
   * Merging
   */

  //!\ becarefull with AGNode use only to read values
  type AGNodeT = DGNode

  //findMergingCandidate find only candidates for interfaces
  //A merging candidate is either structurally equal
  //either a subtype of this
  //hence if we do the merge getNode(nid) will disappear
  // and all its user redirected to the candidate


  override implicit def mergeMatcher(n : ConcreteNode): MergeMatcher =
    InterfaceMergeMatcher.mergeMatcher(n)

  override def findMergingCandidate(g : GraphT, node : ConcreteNode) : Option[ConcreteNode] = {

    val nid = node.id
    node.kind match {
      case Interface if g.content(nid).nonEmpty =>
        g.concreteNodes.find { other =>
          node.canBeMergedInto(other, g) &&
            g.users(nid).forall(!g.interloperOf(_,other.id)) &&
            g.usedBy(nid).forall(!g.interloperOf(other.id, _)
          )
        }
      case _ => None
    }

  }
  def findMergingCandidateIn(g : GraphT, method : ConcreteNode, interface : ConcreteNode) : Option[NodeId] =
    InterfaceMergeMatcher.findMergingCandidateIn(g, method, interface)

}
