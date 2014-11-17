package puck.javaAG.immutable

import puck.graph.AGError
import puck.graph.constraints.{RedirectionPolicy, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.immutable.AccessGraph._
import puck.graph.immutable.constraints.ConstraintsMaps
import puck.graph.immutable.transformations.Recording
import puck.graph.immutable.{NodeKind, AGNode, AGEdge, AccessGraph}
import puck.javaAG.immutable.nodeKind._
import puck.util.PuckLog.InJavaGraph
import puck.util.{PuckLog, PuckNoopLogger, PuckLogger}

import scala.util.{Try, Failure, Success}

/**
 * Created by lorilan on 29/10/14.
 */



class JavaAccessGraph
(program : AST.Program,
 logger : PuckLogger = PuckNoopLogger,
 idSeed : () => Int,
 nodesSet : NodeIndex,
 removedNodes : NodeIndex,
 usersMap : EdgeMap,
 usesMap  : EdgeMap,
 contentsMap  : EdgeMap,
 containerMap : Node2NodeMap,
 superTypesMap : EdgeMap,
 subTypesMap : EdgeMap,
 dominantUsesMap : UseDependencyMap,
 dominatedUsesMap : UseDependencyMap,
 abstractionsMap : AbstractionMap,
 constraints : ConstraintsMaps,
 recording : Recording)
  extends AccessGraph(JavaNode, logger, idSeed, nodesSet, removedNodes,
  usersMap, usesMap, contentsMap, containerMap, superTypesMap, subTypesMap,
  dominantUsesMap, dominatedUsesMap, abstractionsMap, constraints, recording){

  override def newGraph(nLogger : PuckLogger = logger,
               nNodesSet : NodeIndex = nodesSet,
               nRemovedNodes : NodeIndex = removedNodes,
               nUsersMap : EdgeMap = usersMap,
               nUsesMap  : EdgeMap = usesMap,
               nContentMap  : EdgeMap = contentsMap,
               nContainerMap : Node2NodeMap = containerMap,
               nSuperTypesMap : EdgeMap = superTypesMap,
               nSubTypesMap : EdgeMap = subTypesMap,
               nDominantUsesMap : UseDependencyMap = dominantUsesMap,
               nDominatedUsesMap : UseDependencyMap = dominatedUsesMap,
               nAbstractionsMap : AbstractionMap = abstractionsMap,
               nConstraints : ConstraintsMaps = constraints,
               nRecording : Recording = recording) : AccessGraph =
    new JavaAccessGraph(program, nLogger, idSeed,
      nNodesSet, nRemovedNodes, nUsersMap, nUsesMap,
      nContentMap, nContainerMap, nSuperTypesMap, nSubTypesMap,
      nDominantUsesMap, nDominatedUsesMap,
      nAbstractionsMap, nConstraints, nRecording)


  implicit val defaultVerbosity = (InJavaGraph, PuckLog.Info)

  override def coupling = nodes.foldLeft(0 : Double){ (acc, n) => n.kind match {
    case Package =>
      val c = n.coupling
      if(c.isNaN) acc
      else acc + c
    case _ => acc
  }}

  override def abstractionName(implId: NIdT, abskind : NodeKind, policy : AbstractionPolicy) : String = {
    val impl = getNode(implId)
    if (impl.kind == Constructor)
      "create"
    else
      (abskind, policy) match {
        case (Method, SupertypeAbstraction)
             | (AbstractMethod, SupertypeAbstraction) => impl.name
        case _ => super.abstractionName(implId,abskind, policy)

      }
  }

  import puck.util.ErrorHandling.traverse

  override def createAbstraction(implId: NIdT,
                                 abskind : NodeKind ,
                                 policy : AbstractionPolicy) : Try[(NIdT, GraphT)] = {

    (abskind, policy) match {
      case (Interface, SupertypeAbstraction) =>
        val implContent = content(implId)

        val tryAbs = super.createAbstraction( implId,
          Interface,
          SupertypeAbstraction)

        val tryAbs1 = directSuperTypes(implId).foldLeft(tryAbs){
          case (tryAbs0, superType) =>
            tryAbs map {
              case (absId, g) =>
                (absId, g.changeSource(AGEdge.isa(implId, superType), absId))
            }
        }


        tryAbs1 flatMap {
          case (absId, g) =>
            val abs = g.getNode(absId)
            val g2Try = traverse(implContent, g){
              (g0, child) =>
                getNode(child).kind match {
                  //case ck @ Method() =>
                  case ck : MethodKind =>
                    val gAbsTry = g.createAbstraction(child, AbstractMethod,  SupertypeAbstraction)
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
                          logger.writeln("interface creation : redirecting %s target to %s".format(AGEdge.uses(child, implId), abs), 3)
                          g1.redirectUses(AGEdge.uses(child, implId), absId, SupertypeAbstraction) map {
                            case (_, g22) => g22
                          }
                        }
                        else Success(g1)
                      case _ => Success(g0)
                    }
                }

                g4Try map {g4 => (absId, g4)}
            }

            /*g2Try match {
              case Failure(f) => Failure(f)
              case Success(g2) =>
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
                          logger.writeln("interface creation : redirecting %s target to %s".format(AGEdge.uses(child, implId), abs), 3)
                          g1.redirectUses(AGEdge.uses(child, implId), absId, SupertypeAbstraction) map {
                            case (_, g22) => g22
                          }
                        }
                        else Success(g1)
                      case _ => Success(g0)
                    }
                }


                g4Try map {case g4 => (absId, g4)}
            }*/
        }

      case (AbstractMethod, SupertypeAbstraction) =>
        //no (abs, impl) or (impl, abs) uses
        Success(createNode(implId, abskind, policy))

      case _ =>
        val tryAbs = super.createAbstraction(implId, abskind, policy)
         tryAbs flatMap {
           case (abs, g1) =>
             val implNode =g1.getNode(implId)
             val absNode = g1.getNode(abs)
             (implNode.t, absNode.t) match {
               case (ConstructorDeclHolder(cdecl), ConstructorMethodDeclHolder(decl,_)) =>
                  Success(abs, g1.setInternal(abs, ConstructorMethodDeclHolder(decl, cdecl)))
               case (ConstructorDeclHolder(_),_) => Failure(new AGError())
               case _ => Success((abs, g1))
             }
         }
    }
  }

  override def abstractionCreationPostTreatment(implId : NIdT,
                                                absId : NIdT,
                                                policy : AbstractionPolicy) : GraphT = {
    val abstraction = getNode(absId)
    (abstraction.kind, policy) match {
      case (AbstractMethod, SupertypeAbstraction) =>
        val implContainer = container(implId)
        val thisClassNeedsImplement = (abstractions(implContainer) find
          {case (abs, absPolicy) => absPolicy == SupertypeAbstraction &&
            abs == abstraction.container}).isEmpty

        if(!thisClassNeedsImplement) this
        else {
          val absContainer = container(absId)
          val g1 = addUses(implContainer, absContainer)
            .addIsa(implContainer, absContainer)

          g1.content(absId).foldLeft(g1){
            case (g0, absMethodId) => val absMeth = g0.getNode(absMethodId)
              g0.changeType(absMethodId, absMeth.styp, implId, absId)
          }
        }
      case _ => this
    }
  }



  override def redirectUses(oldEdge : EdgeT, newUsee : NIdT,
                   policy : RedirectionPolicy,
                   propagateRedirection : Boolean = true,
                   keepOldUse : Boolean = false ) : Try[(EdgeT, GraphT)] = {

    val tryEdgeGraph =
        super.redirectUses(oldEdge, newUsee, policy,
          propagateRedirection, keepOldUse)

    getNode(oldEdge.usee).kind match {
      case Constructor =>
        tryEdgeGraph map {case (e, g) =>
          (e, users(oldEdge.user).foldLeft(g){ case (g0, userId) =>
              g0.addUses(userId, oldEdge.usee)})
      }
      case _ => tryEdgeGraph
    }
  }

  /*
   * Merging
   */

  //!\ becarefull with AGNode use only to read values
  type AGNodeT = AGNode

  //findMergingCandidate find only candidates for interfaces
  //A merging candidate is either structurally equal
  //either a subtype of this
  //hence if we do the merge getNode(nid) will disappear
  // and all its user redirected to the candidate
  override def findMergingCandidate(nid : NIdT) : Option[NIdT] = {


    def areMergingCandidates(interface1 : AGNodeT, interface2: AGNodeT): Boolean = {

      def hasMatchingMethod(absmId : NIdT) = {
        val absm = getNode(absmId)
        absm.kind match {
          case AbstractMethod => findMergingCandidateIn(absm, interface2).isDefined
          case _ => throw new AGError("Interface should contain only abstract method !!")
        }
      }


      //the two interface are structurally compatible to merge
      interface2.content.size >= interface1.content.size &&
        (interface1.content forall hasMatchingMethod) &&
        (interface2.content.size == interface1.content.size ||
          { interface1.directSubTypes forall interface2.isSuperTypeOf
            //TODO structual type check
            /*val missingMethodsInThis =
              otherItc.content.filterNot{hasMatchingMethodIn(this)}*/
          }) ||
      //the two interfaces introduced an uneeded level of indirection
      interface1.isa(interface2.id) && (interface1.directSubTypes forall interface2.isSuperTypeOf)

    }


    val node = getNode(nid)
    node.kind match {

      case Interface if content(nid).nonEmpty =>
        nodes.find { other =>
           other.kind == Interface && other.id != nid &&
             areMergingCandidates(node, other) &&
                users(nid).forall(!interloperOf(_,other.id)) &&
                usedBy(nid).forall(!interloperOf(other.id, _))
        }.map(_.id)
      case _ => None
    }

  }

  override def findMergingCandidateIn(methodId : NIdT,  interface : NIdT): Option[NIdT] =
    findMergingCandidateIn(getNode(methodId), getNode(interface))


  def findMergingCandidateIn(method : AGNodeT, interface : AGNodeT) = {
    //node.graph.logger.writeln("searching merging candidate for %s".format(node), 8)
    if(method.styp.isEmpty)
      throw new AGError("Method must have a type")

    val mType = method.styp.redirectUses(method.container, interface)

    interface.content.find { ncId =>
      val nc =getNode(ncId)
      nc.kind match {
        case AbstractMethod =>  nc.name == method.name && nc.styp == mType
        case _ => false
      }
    }

  }

  def packageNode(id : NodeId) : NodeId =
    getNode(id).kind match {
      case Package => id
      case _ => packageNode(container(id))
    }

}
