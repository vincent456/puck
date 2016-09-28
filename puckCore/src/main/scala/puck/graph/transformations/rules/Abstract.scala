/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.graph
package transformations.rules

import puck.graph.ShowDG._
import puck.util.LoggedEither._

import scalaz.std.list._
import scalaz.std.set._

abstract class Abstract {

  def abstractionName
  ( g: DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy,
    sUsesAccessKind: Option[UsesAccessKind]
  ) : String =
    impl.name + "_" + policy

  def absType
  ( g : DependencyGraph,
    impl : NodeId,
    sUsesAccessKind: Option[UsesAccessKind]
  ) : Type =
    sUsesAccessKind match {
      case None | Some(Read) => (g styp impl).get
      case Some(Write) => g.nodeKindKnowledge.writeType(g, impl)
      case Some(RW) => sys.error("should not happen")
    }

  def completeReadWriteAbstraction
  (g : DependencyGraph,
   impl : ConcreteNode,
   abs : ReadWriteAbstraction) : DependencyGraph =
    abs  match {
      case ReadWriteAbstraction(Some(r), None) =>

        val absKind = g.getNode(r).kind
        val (g1, wNodeId) = createWriteAbs(g, impl, absKind)
        val setDef = g1.definitionOf_!(wNodeId)
        val c = g.container_!(r)


        g1.addContains(c, wNodeId)
          .removeAbstraction(impl.id, ReadWriteAbstraction(Some(r), None))
          .addAbstraction(impl.id, ReadWriteAbstraction(Some(r), Some(wNodeId)))
          .addBinding((c,c), (setDef, impl.id))
          .addAccessKind(((c,c), (setDef, impl.id)), RW)

      case ReadWriteAbstraction(None, Some(w)) =>

        val absKind = g.getNode(w).kind
        val (g1, rNodeId) = createReadAbs(g, impl, absKind)
        val getDef = g1.definitionOf_!(rNodeId)
        val c = g.container_!(w)

        g1.addContains(c, rNodeId)
          .removeAbstraction(impl.id, ReadWriteAbstraction(None, Some(w)))
          .addAbstraction(impl.id, ReadWriteAbstraction(Some(rNodeId), Some(w)))
          .addBinding((c,c), (getDef, impl.id))
          .addAccessKind(((c,c), (getDef, impl.id)), Read)

      case ReadWriteAbstraction(Some(_), Some(_)) => g
      case _ => puck.error()

    }

  private def createReadAbs
  (g : DependencyGraph,
   impl: ConcreteNode,
   absKind : NodeKind) : (DependencyGraph, NodeId) = {
    import g.nodeKindKnowledge.intro
    val rName = abstractionName(g, impl, absKind, DelegationAbstraction, Some(Read))
    val rType = absType(g, impl.id, Some(Read))
    val (rNode, rDef, g1) = intro.nodeWithDef(g, rName, absKind, rType)

    (g1.addAbstraction(impl.id, ReadWriteAbstraction(Some(rNode.id), None))
      .addTypeConstraint(Sub(TypeOf(impl.id), TypeOf(rNode.id)))
      .addUses(rDef.id, impl.id), rNode.id)
  }

  private def createWriteAbs
  (g : DependencyGraph,
   impl: ConcreteNode,
   absKind : NodeKind) : (DependencyGraph, NodeId) = {
    import g.nodeKindKnowledge.intro
    val wName = abstractionName(g, impl, absKind, DelegationAbstraction, Some(Write))
    val wType = absType(g, impl.id, Some(Write))
    val (wNode, wDef, g1) = intro.nodeWithDef(g, wName, absKind, wType)

    val paramKind = g.nodeKindKnowledge.kindOfKindType(Parameter).head
    val (pNode, g2) = g1.addConcreteNode(impl.name, paramKind)
    val g4 = g.styp(impl.id) map (g2.addType(pNode.id, _)) getOrElse g2
    (g4.addAbstraction(impl.id, ReadWriteAbstraction(None, Some(wNode.id)))
      .addEdge(ContainsParam(wNode.id, pNode.id))
      .addTypeConstraint(Sub(TypeOf(impl.id), TypeOf(wNode.id)))
      .addTypeConstraint(Sub(TypeOf(pNode.id), TypeOf(impl.id)))
      .addUses(wDef.id, impl.id), wNode.id)

  }

  def createAbsNodeAndUse
  (g : DependencyGraph,
   impl: ConcreteNode,
   absKind : NodeKind,
   policy : AbstractionPolicy
  ) : (Abstraction, DependencyGraph) = {
    import g.nodeKindKnowledge.intro

    def copyParameters(g: DependencyGraph, absId : NodeId) =
      g.parametersOf(impl.id).foldRight(g) {
        (paramId, g0) =>
          val param = g0.getConcreteNode(paramId)
         val (pabs, g01) = g0.addConcreteNode(param.name, param.kind)
          val g02 = (g0 styp paramId) map (g01.addType(pabs.id, _)) getOrElse g01
          g02.addEdge(ContainsParam(absId, pabs.id))

      }

    policy match {
      case DelegationAbstraction if impl.kind.isWritable =>

        val (g1, rNodeId) = createReadAbs(g, impl, absKind)
        val (g2, wNodeId) = createWriteAbs(g1, impl, absKind)

        val abs = ReadWriteAbstraction(Some(rNodeId), Some(wNodeId))

        (abs,
          g2.removeAbstraction(impl.id, ReadWriteAbstraction(Some(rNodeId), None))
            .removeAbstraction(impl.id, ReadWriteAbstraction(None, Some(wNodeId)))
            .addAbstraction(impl.id, abs))

      case DelegationAbstraction =>
        val name = abstractionName(g, impl, absKind, policy, None)

        val (absNode, ndef, g1) =
          intro.nodeWithDef(g, name, absKind, g typ impl.id)
        val g2 =
          if(impl.kind.kindType == TypeConstructor) {
            val typ = g container_! impl.id
            g1.setRole(absNode.id, Some(Factory(impl.id)))
              //type returned must be a subtype of the factory type
              .addTypeConstraint(Sub(TypeOf(impl.id), TypeOf(absNode.id)) )
          }
          else g1

        val g3 = copyParameters(g2, absNode.id)
        val g4 = g3.addUses(ndef.id, impl.id)

        val abs = AccessAbstraction(absNode.id, policy)
        (abs, g4.addAbstraction(impl.id, abs))

      case SupertypeAbstraction =>

        val absName = abstractionName(g, impl, absKind, policy, None)
        val (absNode, g1) = g.addConcreteNode(absName, absKind)
        val g2 = (g1 styp impl.id) map (g1.addType(absNode.id, _)) getOrElse g1

        val g3 = copyParameters(g2, absNode.id)
        val abs = AccessAbstraction(absNode.id, policy)
        (abs, g3.addAbstraction(impl.id, abs))

    }
  }



  def insertInTypeHierarchy
  ( g : DependencyGraph,
    subTypeId : NodeId,
    newSuperTypeId : NodeId
  ) : LoggedTG =
    g.directSuperTypesId(subTypeId).foldLoggedEither(g){
      (g0, oldSuperTypedId) =>

        val g1 = g0.removeIsa(NamedType(subTypeId), NamedType(oldSuperTypedId))
          .addIsa(NamedType(newSuperTypeId), NamedType(oldSuperTypedId))

        def extractMethod(typeDeclId : NodeId) : List[(ConcreteNode, Type)] =
          g1.content(typeDeclId).toList map g1.getConcreteNode filter { n =>
            n.kind.kindType == InstanceValue &&
              n.kind.abstractionNodeKinds(SupertypeAbstraction).nonEmpty
          } map (n => (n, g1.styp(n.id).get))

        val subTypeMeths = extractMethod(subTypeId)
        val newSupTypeMeths = extractMethod(newSuperTypeId)
        val oldSupTypeMeths = extractMethod(oldSuperTypedId)

        Type.findAndRegisterOverridedInList(g1, newSupTypeMeths, subTypeMeths){
          Type.ignoreOnImplemNotFound
        } flatMap (
          Type.findAndRegisterOverridedInList(_, oldSupTypeMeths, newSupTypeMeths){
            Type.ignoreOnImplemNotFound
          })

    }

  def redirectTypeUseInParameters
  ( g : DependencyGraph, meth : ConcreteNode,
    typeMapping : List[(ConcreteNode, ConcreteNode)]): LoggedTG = {

    val log = "Abstract.redirectTypeUseInParameters : " +
      s"redirecting type uses of ${meth.name} with $typeMapping\n"


    val ltg = g.parametersOf(meth.id).foldLoggedEither(g){
      (g0, pid) =>
        val typesUsed = typeMapping.filter {case (k, _) => g0.uses(pid, k.id)}
        typesUsed.foldLoggedEither(g0) {
          case (g1, (clazz, interface)) =>
            Redirection.redirectUsesAndPropagate(g0, (pid, clazz.id),
              AccessAbstraction(interface.id, SupertypeAbstraction))
        }
    }
    log <++: ltg
  }

  def redirectTypeUseInParameters
  ( g : DependencyGraph, members : List[ConcreteNode],
    clazz : ConcreteNode, interface : ConcreteNode): LoggedTG =
    members.foldLoggedEither(
      g.comment(s"redirectTypeUseInParameters(g, $members, $clazz, $interface)")){
      (g0, child) =>
        child.kind.kindType match {
          case InstanceValue if g0.parametersOf(child.id).exists(g0.uses(_, clazz.id)) =>
            redirectTypeUseInParameters(g0, child, List((clazz, interface)))
          case _ => LoggedSuccess(g0)
        }
    }


  def canBeAbstracted
  (g : DependencyGraph,
   memberDecl : ConcreteNode,
   clazz : ConcreteNode,
   policy : AbstractionPolicy) : Boolean = {
    //the originSiblingDecl arg is needed in case of cyclic uses
    def aux(originSiblingDecl : ConcreteNode)(memberDecl: ConcreteNode): Boolean = {


      def sibling: NodeId => Boolean =
        sid => g.contains(clazz.id, sid) &&
          sid != originSiblingDecl.id &&
          sid != memberDecl.id

      def usedOnlyViaSelf(user : NodeId, used : NodeId) : Boolean =
        g.typeUsesOf(user, used) forall { case (s, t) => s == t }


      //TODO check if the right part of the and is valid for Delegation abstraction
      memberDecl.kind.canBeAbstractedWith(policy) && {

        val usedNodes = (memberDecl.definition(g) map g.usedByExcludingTypeUse).getOrElse(Set())

        usedNodes.isEmpty || {
          val usedSiblings = usedNodes filter sibling map g.getConcreteNode
          usedSiblings.forall {
            used0 =>
              aux(memberDecl)(used0) || usedOnlyViaSelf(memberDecl.definition_!(g), used0.id)
          }
        }
      }
    }

    aux(memberDecl)(memberDecl)
  }


  def typeMembersToPutInInterface
  ( g : DependencyGraph,
    clazz : ConcreteNode,
    policy : AbstractionPolicy) : List[ConcreteNode] = {

    g.content(clazz.id).foldLeft(List[ConcreteNode]()){
      (acc, mid) =>
        val member = g.getConcreteNode(mid)
        if(canBeAbstracted(g, member, clazz, policy)) member +: acc
        else acc
    }
  }


  def abstractTypeDeclAndReplaceByAbstractionWherePossible
  ( g : DependencyGraph,
    clazz : ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy,
    members : List[ConcreteNode]
  ) : LoggedTry[(Abstraction, DependencyGraph)] = {

    def createAbstractTypeMemberWithSuperSelfType
    ( g : DependencyGraph,
      meth : ConcreteNode,
      interface : ConcreteNode,
      typeMapping : List[(ConcreteNode, ConcreteNode)]
    ) : LoggedTG ={
      createAbstraction(g, meth, meth.kind.abstractionNodeKinds(policy).head, policy) flatMap {
        case (AccessAbstraction(absMethodId, _), g0) =>

          val typesToRedirect =
            typeMapping filter {case (k, _) => g0.uses(absMethodId, k.id)}

          val g2 =
            typesToRedirect.foldLeft(g0.addContains(interface.id, absMethodId)) {
              case (g1, (classType, interfaceType)) =>
              //is change type needed in case of delegation policy ?
              g1.changeType((absMethodId, classType.id), interfaceType.id) //change return type
            }

          val absMeth = g2 getConcreteNode absMethodId
          redirectTypeUseInParameters(g2, absMeth, typeMapping)

        case _ => LoggedError("unexpected type of abstraction")
      }

    }

    val (interfaceAbs, g1) = createAbsNodeAndUse(g, clazz, abskind, policy)


    val interface = interfaceAbs match {
      case AccessAbstraction(id, _) => g1.getConcreteNode(id)
      case _ => sys.error("type should not have a RW abstraction")
    }

    val typeVariablesMap =
      g1.parametersOf(clazz.id).zip(g1.parametersOf(interface.id)) map {
        case (id1, id2) => (g1 getConcreteNode id1, g1 getConcreteNode id2)
      }

    val typeMapping = (clazz, interface) :: typeVariablesMap

    def addIsa(g : DependencyGraph) : DependencyGraph = {
      val ps = g.parametersOf(clazz.id)
      if (ps.isEmpty) g.addIsa(NamedType(clazz.id), NamedType(interface.id))
      else {
        val l = ps.foldRight(List[Type]())(NamedType(_) :: _)
        g.addIsa(NamedType(clazz.id), ParameterizedType(interface.id, l))
      }
    }


    for {

      g2 <- members.foldLoggedEither(g1)(
        createAbstractTypeMemberWithSuperSelfType(_, _, interface, typeMapping))

      g3 <- policy match {
        case SupertypeAbstraction => insertInTypeHierarchy(g2, clazz.id, interface.id)
        case DelegationAbstraction => LoggedSuccess(g2)
      }

      g4 <- if(policy == SupertypeAbstraction)
        redirectTypeUseInParameters(addIsa(g3), members, clazz, interface)
      else LoggedSuccess(g3)


      log = s"interface $interface created, contains : {" +
        g4.content(interface.id).map(nid => (g4, nid).shows).mkString("\n")+
        "}"
      g5 <- LoggedSuccess(log, g4)

    } yield (interfaceAbs, g5)
  }

  def createAbstraction
  ( graph : DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind ,
    policy : AbstractionPolicy
  ) : LoggedTry[(Abstraction, DependencyGraph)] ={
    val g = graph.comment(s"Abstract.createAbstraction(g, ${(graph,impl).shows}, $abskind, $policy)")
    (abskind.kindType, policy) match {
      case (TypeDecl, SupertypeAbstraction) =>
        val methods = typeMembersToPutInInterface(g, impl, SupertypeAbstraction)
        val log = s"Creating $abskind with abstractions of" +
          methods.mkString("\n", "\n", "\n")

        val ltg = abstractTypeDeclAndReplaceByAbstractionWherePossible(g,
          impl,
          abskind, SupertypeAbstraction,
          methods)

        log <++: ltg
      case (TypeDecl, DelegationAbstraction) =>

        val methods = g.content(impl.id).foldLeft(List[ConcreteNode]()){
          (acc, mid) =>
            val member = g.getConcreteNode(mid)
            if(member.kind canBeAbstractedWith DelegationAbstraction) member +: acc
            else acc
        }

        abstractTypeDeclAndReplaceByAbstractionWherePossible(g,
          impl, abskind, DelegationAbstraction, methods)

      case _ => LoggedSuccess(createAbsNodeAndUse(g, impl, abskind, policy))
    }
  }



}
