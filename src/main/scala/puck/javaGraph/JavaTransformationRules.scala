package puck.javaGraph

import puck.PuckError
import puck.graph._
import puck.graph.constraints.{Move, RedirectionPolicy, SupertypeAbstraction, AbstractionPolicy}
import puck.graph.transformations.TransformationRules
import puck.javaGraph.nodeKind._

import scalaz._
import scalaz.Validation.FlatMap._

/**
 * Created by lorilan on 25/01/15.
 */
object JavaTransformationRules extends TransformationRules {

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

  //TODO see if it can be rewritten using scalaz !
  def traverse[A, B, E](a: Iterable[A], b: B)(f: (B, A) => Validation[E, B]): Validation[E,B] =
    a.foldLeft(Success(b): Validation[E,B]){case (b0, a0) =>
      if(b0.isSuccess) b0 flatMap (f(_, a0))
      else b0
    }



  def insertInTypeHierarchy(g : GraphT, classId : NIdT, interfaceId : NIdT) : GraphT =
    g.directSuperTypes(classId).foldLeft(g){ (g0, superType) =>
      g0.changeSource(DGEdge.isa(classId, superType), interfaceId)
    }

  def addTypesUses(g : GraphT, nodeId : NIdT) : GraphT = {
    val typesUsed = g.getNode(nodeId).styp.getTypeNodeIds
    typesUsed.foldLeft(g){(g0, tid) => g0.addUses(nodeId, tid)}
  }

  def addAbstractMethodAndRedirectSelfType
    (g : GraphT, methId : NodeId, itcId : NodeId, classId : NodeId):  Try[GraphT] ={
    val methodNode = g.getNode(methId)
    methodNode.kind match {
      case AbstractMethod =>
        val g2 = g.addContains(itcId, methId)
        //TODO check why it is not needed
        //addTypesUses(g4, absChild)
          .changeType(methId, methodNode.styp, classId, itcId)
        Success(g2)
      case k => Failure(new DGError(k + " should be an abstract method !")).toValidationNel
    }
  }

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

        val tryAbs1 = tryAbs.map { case (absId, g0) => (absId, insertInTypeHierarchy(g0, implId, absId))}

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
                        addAbstractMethodAndRedirectSelfType(g21, absChild, absId, implId)
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
                        g.logger.writeln(s"interface creation : redirecting ${DGEdge.uses(child, implId)} target to $abs")
                        redirectUsesOf(g1, DGEdge.uses(child, implId), absId, SupertypeAbstraction) map {
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
      case (ConstructorMethod, _) =>
        super.createAbstraction(g, implId, abskind, policy) map { case (absId, g0) =>
          (absId, addTypesUses(g0, absId))
        }
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

  def redirectThisTypeUse(g : GraphT, thisType : NIdT, movedId : NIdT): Try[(EdgeT, GraphT)] = {
    val typeNode = g.getNode(thisType)
    val movedNode = g.getNode(movedId)
    typeNode.kind match {
      case Class =>
        val newTypeUsed = findNewTypeUsed(g, thisType, movedId, Move)
        val (fid, g2) = g.addNode(movedNode.name + "_delegate", Field, NamedTypeHolder(new JavaNamedType(newTypeUsed)))
        val g3 = g2.addContains(thisType, fid)
              .addUses(fid, newTypeUsed)
              .addUses(movedId, fid)
        Success( (DGEdge.uses(fid, newTypeUsed),g3))
      case _=>
        Failure(new PuckError(s"redirect type uses, expected class got ${typeNode.kind}")).toValidationNel
    }
  }


  override def redirectUsesOf(g : GraphT,
                            oldEdge : EdgeT, newUsee : NIdT,
                            policy : RedirectionPolicy,
                            propagateRedirection : Boolean = true,
                            keepOldUse : Boolean = false ) : Try[(EdgeT, GraphT)] = {

    val tryEdgeGraph =
      super.redirectUsesOf(g, oldEdge, newUsee, policy,
        propagateRedirection, keepOldUse)

    g.getNode(oldEdge.used).kind match {
      case Constructor =>
        tryEdgeGraph map {case (e, g0) =>
          (e, g.users(oldEdge.user).foldLeft(g0){ case (g1, userId) =>
            g1.addUses(userId, oldEdge.used)})
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
          case _ => throw new DGError("Interface should contain only abstract method !!")
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


  def findMergingCandidateIn(g : GraphT, method : AGNodeT, interface : AGNodeT) : Option[NIdT] = {
    //node.graph.logger.writeln("searching merging candidate for %s".format(node), 8)
    if(method.styp.isEmpty)
      throw new DGError("Method must have a type")

    val mType = method.styp.redirectUses(g.container(method.id).get, interface)
    g.content(interface.id).find { ncId =>
      val nc = g.getNode(ncId)
      nc.kind match {
        case AbstractMethod => nc.name == method.name && nc.styp == mType
        case _ => false
      }
    }
  }
}
