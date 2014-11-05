package puck.javaAG.immutable

import puck.graph.AGError
import puck.graph.constraints.{SupertypeAbstraction, AbstractionPolicy}
import puck.graph.immutable.AccessGraph._
import puck.graph.immutable.constraints.ConstraintsMaps
import puck.graph.immutable.transformations.Recording
import puck.graph.immutable.{AGEdge, AccessGraph}
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
 nodesSet : NodeIndex[JavaNodeKind, DeclHolder],
 usersMap : EdgeMap[JavaNodeKind],
 usesMap  : EdgeMap[JavaNodeKind],
 contentsMap  : EdgeMap[JavaNodeKind],
 containerMap : Node2NodeMap[JavaNodeKind],
 superTypesMap : EdgeMap[JavaNodeKind],
 subTypesMap : EdgeMap[JavaNodeKind],
 dominantUsesMap : UseDependencyMap[JavaNodeKind],
 dominatedUsesMap : UseDependencyMap[JavaNodeKind],
 abstractionsMap : AbstractionMap[JavaNodeKind],
 constraints : ConstraintsMaps[JavaNodeKind],
 recording : Recording[JavaNodeKind, DeclHolder])
  extends AccessGraph[JavaNodeKind, DeclHolder](JavaNode, logger, idSeed, nodesSet,
  usersMap, usesMap, contentsMap, containerMap, superTypesMap, subTypesMap,
  dominantUsesMap, dominatedUsesMap, abstractionsMap, constraints, recording){

  override def newGraph(nLogger : PuckLogger = logger,
               nNodesSet : NodeIndex[JavaNodeKind, DeclHolder] = nodesSet,
               nUsersMap : EdgeMap[JavaNodeKind] = usersMap,
               nUsesMap  : EdgeMap[JavaNodeKind] = usesMap,
               nContentMap  : EdgeMap[JavaNodeKind] = contentsMap,
               nContainerMap : Node2NodeMap[JavaNodeKind] = containerMap,
               nSuperTypesMap : EdgeMap[JavaNodeKind] = superTypesMap,
               nSubTypesMap : EdgeMap[JavaNodeKind] = subTypesMap,
               nDominantUsesMap : UseDependencyMap[JavaNodeKind] = dominantUsesMap,
               nDominatedUsesMap : UseDependencyMap[JavaNodeKind] = dominatedUsesMap,
               nAbstractionsMap : AbstractionMap[JavaNodeKind] = abstractionsMap,
               nConstraints : ConstraintsMaps[JavaNodeKind] = constraints,
               nRecording : Recording[JavaNodeKind, DeclHolder] = recording) : AccessGraph[JavaNodeKind, DeclHolder] =
    new JavaAccessGraph(program, nLogger, idSeed,
      nNodesSet, nUsersMap, nUsesMap,
      nContentMap, nContainerMap, nSuperTypesMap, nSubTypesMap,
      nDominantUsesMap, nDominatedUsesMap,
      nAbstractionsMap, nConstraints, nRecording)


  type NK = JavaNodeKind
  override type NIdT = NodeId[NK]

  implicit val defaultVerbosity = (InJavaGraph, PuckLog.Info)

  override def abstractionName(implId: NIdT, abskind : NK, policy : AbstractionPolicy) : String = {
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
                                 abskind : NK ,
                                 policy : AbstractionPolicy) : Try[(NIdT, GraphT)] = {

    (abskind, policy) match {
      case (Interface, SupertypeAbstraction) =>
        val implContent = content(implId)
        super.createAbstraction( implId,
                                 Interface,
                                 SupertypeAbstraction) match {
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
                }            }
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

  //other is potentialy a superType and thus may have less methods than this
 /* override def mergeWith(other : AGNode[JavaNodeKind]){
    (this.kind, other.kind) match {
      case (Interface(), Interface()) =>
        //TODO see why the call to apply on content is needed (other.content.toList compile but doesn't give the right result)
        //the toList is necessary :
        // removing the firsts children (AGEdge.contains(other.container, other).delete() in AGNode.mergeWith)
        // modifies the content set and thus the iterator
        other.content().toList foreach { otherAbsMethod =>
          otherAbsMethod.kind match {
            case otherKind @ AbstractMethod() =>
              otherKind.findMergingCandidate(this) match {
                case Some(thisAbsMethod) => thisAbsMethod mergeWith otherAbsMethod
                case None => throw new Error(otherAbsMethod.fullName + " has no method to merge with")
              }
            case _ => assert(false)
          }
        }
        super.mergeWith(other)

      case (AbstractMethod(), AbstractMethod()) =>
        super.mergeWith(other)
      case _ => ()
    }
  }*/
}
