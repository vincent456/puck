package puck.javaAG

import puck.graph._
import puck.graph.constraints.{RedirectionPolicy, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.transformations.TransformationRules
import puck.javaAG.nodeKind._

import scala.util.{Failure, Success, Try}

/**
 * Created by lorilan on 25/01/15.
 */
object JavaTransformationRules extends TransformationRules{

  override def abstractionName( g: GraphT, implId: NIdT, abskind : NodeKind, policy : AbstractionPolicy) : String = {
    val impl = g.getNode(implId)
    if (impl.kind == Constructor)
      "create"
    else
      (abskind, policy) match {
        case (Method, SupertypeAbstraction)
             | (AbstractMethod, SupertypeAbstraction) => impl.name
        case _ => super.abstractionName(g, implId, abskind, policy)

      }
  }

  import puck.util.ErrorHandling.traverse

  override def createAbstraction(g : GraphT,
                                 implId: NIdT,
                                 abskind : NodeKind ,
                                 policy : AbstractionPolicy) : Try[(NIdT, GraphT)] = {

    (abskind, policy) match {
      case (Interface, SupertypeAbstraction) =>
        val implContent = g.content(implId)

        val tryAbs = super.createAbstraction(g, implId,
          Interface,
          SupertypeAbstraction)

        val tryAbs1 = g.directSuperTypes(implId).foldLeft(tryAbs){
          case (tryAbs0, superType) =>
            tryAbs map {
              case (absId, g0) =>
                (absId, g0.changeSource(DGEdge.isa(implId, superType), absId))
            }
        }

        tryAbs1 flatMap {
          case (absId, g1) =>
            val abs = g1.getNode(absId)
            val g2Try = traverse(implContent, g1){
              (g0, child) =>
                g.getNode(child).kind match {
                  //case ck @ Method() =>
                  case ck : MethodKind =>
                    val gAbsTry = createAbstraction(g1, child, AbstractMethod,  SupertypeAbstraction)
                    gAbsTry flatMap {
                      case (absChild, g21) =>
                        val g3 = g21.addContains(absId, absChild)
                        val absChildNode = g3.getNode(absChild)
                        absChildNode.kind match {
                          case AbstractMethod =>
                            Success(g3.changeType(absChild, absChildNode.styp, implId, absId))
                          case k => Failure(new AGError(k + " should be an abstract method !"))
                        }
                    }
                  case _ => Success(g0)
                }
            }

            g2Try flatMap { g2 =>
              val g3 = g2.addIsa(implId, absId)

              val g4Try = traverse(implContent, g3){
                case (g0, child) =>
                  val node = g0.getNode(child)
                  (node.kind, node.styp) match {
                    // even fields can need to be promoted if they are written
                    //case Field() =>
                    case (ck : MethodKind, MethodTypeHolder(typ))  =>

                      val g1 = g0.changeContravariantType(child, node.styp, implId, abs.id)

                      if(g1.uses(child, implId)) {
                        g.logger.writeln("interface creation : redirecting %s target to %s".format(DGEdge.uses(child, implId), abs), 3)
                        redirectUses(g1, DGEdge.uses(child, implId), absId, SupertypeAbstraction) map {
                          case (_, g22) => g22
                        }
                      }
                      else Success(g1)
                    case _ => Success(g0)
                  }
              }

              g4Try map {g4 => (absId, g4)}
            }
        }

      case (AbstractMethod, SupertypeAbstraction) =>
        //no (abs, impl) or (impl, abs) uses
        Success(createNode(g, implId, abskind, policy))

      case _ => super.createAbstraction(g, implId, abskind, policy)
    }
  }

  override def abstractionCreationPostTreatment(g: GraphT,
                                                implId : NIdT,
                                                absId : NIdT,
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
            case (g0, absMethodId) => val absMeth = g0.getNode(absMethodId)
              g0.changeType(absMethodId, absMeth.styp, implId, absId)
          }
        }
      case _ => g
    }
  }



  override def redirectUses(g : GraphT,
                            oldEdge : EdgeT, newUsee : NIdT,
                            policy : RedirectionPolicy,
                            propagateRedirection : Boolean = true,
                            keepOldUse : Boolean = false ) : Try[(EdgeT, GraphT)] = {

    val tryEdgeGraph =
      super.redirectUses(g, oldEdge, newUsee, policy,
        propagateRedirection, keepOldUse)

    g.getNode(oldEdge.usee).kind match {
      case Constructor =>
        tryEdgeGraph map {case (e, g0) =>
          (e, g.users(oldEdge.user).foldLeft(g0){ case (g1, userId) =>
            g1.addUses(userId, oldEdge.usee)})
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
  override def findMergingCandidate(g : GraphT, nid : NIdT) : Option[NIdT] = {


    def areMergingCandidates(interface1 : NodeId, interface2: NodeId): Boolean = {

      def hasMatchingMethod(absmId : NIdT) = {
        val absm = g.getNode(absmId)
        absm.kind match {
          case AbstractMethod => findMergingCandidateIn(g, absm, g.getNode(interface2)).isDefined
          case _ => throw new AGError("Interface should contain only abstract method !!")
        }
      }


      //the two interface are structurally compatible to merge
      g.content(interface2).size >= g.content(interface1).size &&
        (g.content(interface1) forall hasMatchingMethod) &&
        (g.content(interface2).size == g.content(interface1).size ||
          { g.directSubTypes(interface1).forall(g.isSuperTypeOf(interface2,_))
            //TODO structual type check
            /*val missingMethodsInThis =
              otherItc.content.filterNot{hasMatchingMethodIn(this)}*/
          }) ||
        //the two interfaces introduced an uneeded level of indirection
        g.isa(interface1, interface2) &&
          g.directSubTypes(interface1).forall(g.isSuperTypeOf(interface2,_))

    }


    val node = g.getNode(nid)
    node.kind match {

      case Interface if g.content(nid).nonEmpty =>
        g.nodes.find { other =>
          other.kind == Interface && other.id != nid &&
            areMergingCandidates(nid, other.id) &&
            g.users(nid).forall(!g.interloperOf(_,other.id)) &&
            g.usedBy(nid).forall(!g.interloperOf(other.id, _))
        }.map(_.id)
      case _ => None
    }

  }

  override def findMergingCandidateIn(g : GraphT, methodId : NIdT,  interfaceId : NIdT): Option[NIdT] =
    findMergingCandidateIn(g, g.getNode(methodId), g.getNode(interfaceId))


  def findMergingCandidateIn(g : GraphT, method : AGNodeT, interface : AGNodeT) = {
    //node.graph.logger.writeln("searching merging candidate for %s".format(node), 8)
    if(method.styp.isEmpty)
      throw new AGError("Method must have a type")

    val mType = method.styp.redirectUses(g.container(method.id).get, interface)

    g.content(interface.id).find { ncId =>
      val nc = g.getNode(ncId)
      nc.kind match {
        case AbstractMethod =>  nc.name == method.name && nc.styp == mType
        case _ => false
      }
    }

  }
}
