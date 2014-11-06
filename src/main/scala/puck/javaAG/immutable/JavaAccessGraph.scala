package puck.javaAG.immutable

import puck.graph.AGError
import puck.graph.constraints.{SupertypeAbstraction, AbstractionPolicy}
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
  override def createAbstraction(implId: NIdT,
                                 abskind : NodeKind ,
                                 policy : AbstractionPolicy) : Try[(NIdT, GraphT)] = {

    (abskind, policy) match {
      case (Interface, SupertypeAbstraction) =>
        val implContent = content(implId)
        super.createAbstraction( implId,
                                 Interface,
                                 SupertypeAbstraction) match {
          case Failure(exc) => Failure(exc)
          case Success ((absId, g)) =>
            val abs = g.getNode(absId)
            val g2Try = implContent.foldLeft(Success(g) : Try[GraphT]){
              case (Success(g0), child) =>
              getNode(child).kind match {
                //case ck @ Method() =>
                case ck : MethodKind =>
                   val gAbsTry = g.createAbstraction(child, AbstractMethod,  SupertypeAbstraction)
                   gAbsTry match {
                      case Success((absChild, g21)) =>
                        val g3 = g21.addContains(absId, absChild)
                        val absChildNode = g3.getNode(absChild)
                        absChildNode.kind match {
                          case AbstractMethod =>
                            Success(g3.changeType(absChild, absChildNode.styp, implId, absId))
                          case k => Failure(new AGError(k + " should be an abstract method !"))
                        }
                      case Failure(exc) => Failure(exc)
                    }

                //case AbstractMethod() => throw new AGError("unhandled case !")
                case _ => Success(g0)
              }
              case (Failure(f),_) => Failure(f)
            }
            g2Try match {
              case Failure(f) => Failure(f)
              case Success(g2) =>
                val g3 = g2.addIsa(implId, absId)

                val g4Try = implContent.foldLeft(Success(g3) : Try[GraphT]) {
                  case (Success(g0), child) =>
                  val node = g0.getNode(child)
                  (node.kind, node.styp) match {
                    // even fields can need to be promoted if they are written
                    //case Field() =>
                    case (ck : MethodKind, MethodTypeHolder(typ))  =>

                      val newTyp = typ.redirectContravariantUses(implId, abs)
                      val g1 = g0.setType(child, MethodTypeHolder(newTyp))

                      if(g1.uses(child, implId)) {
                        logger.writeln("interface creation : redirecting %s target to %s".format(AGEdge.uses(child, implId), abs), 3)
                        g1.redirectUses(AGEdge.uses(child, implId), absId, SupertypeAbstraction) match {
                          case Success((_, g22)) => Success(g22)
                          case Failure(f) => Failure(f)
                        }
                      }
                      else Success(g1)
                    case _ => Success(g0)
                  }
                  case (Failure(f), _) => Failure(f)
                }
                g4Try match {
                  case Success(g4) => Success((absId, g4))
                  case Failure(f) => Failure(f)
                }
            }
        }



      case (AbstractMethod, SupertypeAbstraction) =>
        //no (abs, impl) or (impl, abs) uses
        val n = getNode(implId)
        val (id, g0) = createNode(implId, abskind, policy)
        Success((id, g0.setType(id, n.styp)))

      case _ =>
        super.createAbstraction(implId, abskind, policy) match {
          case Failure(f) => Failure(f)
          case Success ((abs, g1)) =>
              val implNode =g1.getNode(implId)
              val absNode = g1.getNode(abs)
              (implNode.kind, absNode.kind) match {
                case (Constructor, ConstructorMethod) =>
                  //         case (Constructor(_, _, cdecl), ConstructorMethod(_, typ, decl, _)) =>
                  (implNode.t, absNode.t) match {
                    /*case (ConstructorDeclHolder(cdecl), ConstructorMethodDecl(decl,_)) =>
                      g1.setInternal(abs, ConstructorMethodDecl(decl, cdecl))*/
                    case (ConstructorDeclHolder(cdecl), EmptyDeclHolder) =>
                      Success((abs, g1.setInternal(abs, ConstructorMethodDecl(None, cdecl))))
                    case (_,_) => Failure(new AGError())
                  }
                case (Constructor, _) => Failure(throw new AGError())
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
          val g1 = addUses(implContainer, absContainer).addIsa(implContainer, absContainer)

          g1.content(absId).foldLeft(g1){
            case (g0, absMethodId) => val absMeth = g0.getNode(absMethodId)
              g0.changeType(absMethodId, absMeth.styp, implId, absId)
          }
        }
      case _ => this
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


    def areMergingCandidates(n1 : AGNodeT, n2: AGNodeT): Boolean = {

      def hasMatchingMethod(absmId : NIdT) = {
        val absm = getNode(absmId)
        absm.kind match {
          case AbstractMethod => findMergingCandidateIn(absm, n2).isDefined
          case _ => throw new AGError("Interface should contain only abstract method !!")
        }
      }

      n2.content.size >= n1.content.size &&
        (n1.content forall hasMatchingMethod) &&
        (n2.content.size == n1.content.size ||
          { //otherItc has more methods, it is a potential subtype
            n1.subTypes forall n2.isSuperTypeOf
            //TODO structual type check
            /*val missingMethodsInThis =
              otherItc.content.filterNot{hasMatchingMethodIn(this)}*/
          })
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
  //"consumed" is potentialy a superType and thus may have less methods than this
  override def merge(consumerId : NIdT, consumedId : NIdT) : GraphT = {
    val consumer = getNode(consumedId)
    val consumed = getNode(consumedId)
    (consumer.kind, consumed.kind) match {
      case (Interface, Interface) =>
        //TODO see why the call to apply on content is needed (other.content.toList compile but doesn't give the right result)
        //the toList is necessary :
        // removing the firsts children (AGEdge.contains(other.container, other).delete() in AGNode.mergeWith)
        // modifies the content set and thus the iterator
        val g1 = super.merge(consumerId, consumedId)
        consumed.content.foldLeft(g1) {
          case (g0, consumedAbsMethodId) =>
            val consumedAbsMethod = getNode(consumedAbsMethodId)

            consumedAbsMethod.kind match {
            case AbstractMethod =>
              findMergingCandidateIn(consumedAbsMethod, consumer) match {
                case Some(thisAbsMethod) => g0.merge(thisAbsMethod, consumedAbsMethodId)
                case None => throw new AGError(consumedAbsMethod.fullName + " has no method to merge with")
              }
            case _ => throw new AGError("interface must have only abstract method")
          }
        }

      case (AbstractMethod, AbstractMethod) =>
        super.merge(consumerId, consumedId)
      case _ => this
    }
  }
}
