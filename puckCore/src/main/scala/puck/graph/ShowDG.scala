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

import puck.graph.DependencyGraph.AbstractionMap
import puck.graph.constraints.ShowConstraints
import puck.graph.transformations._

import scala.math.Ordering


object ShowDG extends ShowConstraints{

  def tailRecStringOfType
  (dg : DependencyGraph,
   sep : List[String], end : List[String],
   builder : StringBuilder, lt : List[List[Type]]) : String =
    if (lt.isEmpty) builder.toString()
    else lt.head match {
      case Nil => tailRecStringOfType(dg, sep.tail, end.tail, builder append end.head, lt.tail)
      case NamedType(nid) :: tl =>
        val sep0 =
          if(tl.nonEmpty) sep.head
          else ""
        tailRecStringOfType(dg, sep, end,
          builder.append(dg.getNode(nid).name(dg) + sep0),
          tl :: lt.tail )
      case Tuple(types) :: tl =>
        tailRecStringOfType(dg, "," :: sep, (")" + sep.head) :: end,
          builder append "(", types :: tl :: lt.tail )
      case Arrow(in, out) :: tl =>
        tailRecStringOfType(dg, " -> " :: sep, "" :: end, builder, List(in, out) :: tl :: lt.tail )

      case ParameterizedType(genId, targs) :: tl =>
        val genName = dg.getNode(genId).name(dg)
        tailRecStringOfType(dg, "," :: sep, (">" + sep.head) :: end,
          builder append s"$genName<", targs :: tl :: lt.tail )

      case Covariant(t) :: tl =>
        tailRecStringOfType(dg,  sep,  end,
          builder append s"+", (t :: tl) :: lt.tail )
      case Contravariant(t) :: tl =>
        tailRecStringOfType(dg,  sep,  end,
          builder append s"-", (t :: tl) :: lt.tail )

    }

  implicit def stringOfType : DGStringBuilder[Type] = (dg, t) =>
    tailRecStringOfType(dg, List(""), List(""), new StringBuilder, List(List(t)))

  implicit def stringOfTypeOption : DGStringBuilder[Option[Type]] = (dg, th) => th match {
    case None => ""
    case Some(t) => " : " + stringOfType(dg, t)
  }

  implicit def stringOfNodeId : DGStringBuilder[NodeId] =
    (dg, nid) => stringOfNode(dg, dg.getNode(nid))

  implicit def stringOfNodeIdP : DGStringBuilder[NodeIdP] =
  {case (dg, (nid1, nid2)) => s"Edge( ${desambiguatedFullName(dg,nid1)}, ${desambiguatedFullName(dg,nid2)})"}

  // /!\ this method is used to build mapping based on full names, modifying it can break a LOT of tests !
  implicit def stringOfNode : DGStringBuilder[DGNode] = (dg, n) =>
    n match {
      case n : ConcreteNode =>
        val name =
          if(n.kind.kindType == ValueDef)
            dg.container(n.id) map {
              dg.getConcreteNode(_).name + DependencyGraph.scopeSeparator + n.name
            } getOrElse "OrphanDefinition"
          else n.name
        s"${n.id} - ${n.kind} $name${stringOfTypeOption(dg, dg.styp(n.id))}"
      case vn : VirtualNode => s"${vn.id} - ${vn.name(dg)}"
    }

  def nodeNameTyp : DGStringBuilder[DGNode] =
    (dg, n) => n match {
      case cn : ConcreteNode => cn.name + stringOfTypeOption(dg, dg.styp(cn.id))
      case _ => n.name(dg)
    }


  def desambiguatedLocalName : DGStringBuilder[DGNode] = (g, n) => {
    n.kind.kindType match {
      case StableValue
           | InstanceValue
           | TypeConstructor =>
        g.structuredType(n.id) match {
          case Some(Arrow(in, _)) =>
            n.name + stringOfType(g, in)
          case _ => n.name
        }
      case _ =>  n.name
    }
  }
  def desambiguatedFullName : DGStringBuilder[NodeId] = (g, n) => {
    val ss = DependencyGraph.scopeSeparator
    def aux(nid: NodeId, accu: String): String = {
      val n = g.getNode(nid)
      g.container(n.id) match {
        case None if n.id == g.rootId =>
          if(accu.isEmpty) g.root.name
          else accu.substring(1)
        case None => DependencyGraph.unrootedStringId + ss + n.name + accu
        case Some(pid) =>
          aux(pid, ss + desambiguatedLocalName(g, n) + accu)
      }
    }
    aux(n, "")
  }


  def edgeFullName : DGStringBuilder[DGEdge] = (g, e) =>
    s"${e.kind}(${e.source} - ${g.fullName(e.source)}, ${e.target} - ${g.fullName(e.target)})"


  implicit def stringOfEdge : DGStringBuilder[DGEdge] = (dg, e) =>
    s"${e.kind}( ${desambiguatedFullName(dg, e.source)}, ${desambiguatedFullName(dg, e.target)})"

  implicit def stringOfBR : DGStringBuilder[(NodeIdP,NodeIdP)] =
  {case (dg, (u1, u2)) => s"(${stringOfNodeIdP(dg,u1)}, ${stringOfNodeIdP(dg,u2)})"}


  implicit def stringOfExtremity : DGStringBuilder[Extremity] =
    (dg, e) => s"${e.productPrefix}(${stringOfNode(dg, dg.getNode(e.node))})"

  implicit def stringOfOperation : DGStringBuilder[Operation] = (dg, tgt) =>
    tgt match {
      case CNode(n) => stringOfNode(dg, n)
      case VNode(n) =>stringOfNode(dg, n)
      case Edge(edge) => stringOfEdge(dg, edge)
      case RedirectionOp(edge, Target(newTgt))
        if dg.kindType(edge.target) == TypeDecl =>

        val typed = stringOfNodeId(dg, edge.source)
        val oldType = stringOfNodeId(dg, edge.target)
        val newType =  stringOfNodeId(dg, newTgt)
        s"TypeChange($typed, $oldType, $newType)"

      case RedirectionOp(edge, exty) =>
        val ecord = stringOfEdge(dg, edge)
        val xcord = stringOfExtremity(dg, exty)
        s"${tgt.productPrefix}($ecord, $xcord)"
      case TypeBinding((n1,n2), (n3,n4)) =>
        s"${tgt.productPrefix}(Uses(${stringOfNodeId(dg, n1)}, ${stringOfNodeId(dg,n2)})," +
          s"Uses(${stringOfNodeId(dg, n3)}, ${stringOfNodeId(dg,n4)}))"

      case AType(typed, t) =>
        val typedCord = stringOfNodeId(dg, typed)
        val tcord : String = stringOfType(dg, t)
        s"SetType($typedCord, $tcord)"

      case ChangeTypeOp(typed, oldType, newType) =>
        val typedCord = stringOfNodeId(dg, typed)
        val oldTypeCord = stringOfNodeId(dg, oldType)
        val newTypeCord = stringOfNodeId(dg, newType)

        s"ChangeType($typedCord, $oldTypeCord, $newTypeCord)"

      case ChangeTypeBindingOp((tUse, tmUse), exty) =>
        val tUseCord = stringOfNodeIdP(dg, tUse)
        val tmUseCord = stringOfNodeIdP(dg, tmUse)
        s"ChangeTypeBinding(($tUseCord, $tmUseCord),${exty.productPrefix}(${stringOfNodeIdP(dg, exty.edge)})"
      case Isa(sub, sup) =>
        s"Isa(${stringOfType(dg, sub)}, ${stringOfType(dg, sup)})"
      case _ => tgt.toString
    }

  val directAddRm : Direction => String = {
    case Regular => "Add"
    case Reverse => "Remove"
  }

  val tcOp : TypeConstraint => String = {
    case Sub(_, _) => "<:"
    case Eq(_, _) =>  ":="
  }

  def tcv(dg : DependencyGraph, tv : TypeConstraintVariable) : String = tv match {
    case TypeOf(id) =>
      if(dg.styp(id).isEmpty) s"(${desambiguatedFullName(dg, id)}) should be typed !!"
      else s"(typeOf(${desambiguatedFullName(dg, id)}) = ${stringOfType(dg, dg.typ(id))})"
    case TypeVar(t) =>  stringOfType(dg, t)
    case ParTypeProjection(v, idx) => s"Proj(${tcv(dg, v)}, $idx)"
  }

  def stringOfBinaryTypeConstraint : DGStringBuilder[BinaryTypeConstraint] =
    (dg, tc) => s"${tcv(dg, tc.left)} ${tcOp(tc)} ${tcv(dg, tc.right)}"

  implicit def stringOfTypeConstraint : DGStringBuilder[TypeConstraint] = {
    case (dg, btc : BinaryTypeConstraint) => stringOfBinaryTypeConstraint(dg, btc)
    case (dg, AndTypeConstraint(cts)) =>
    val strb = new StringBuilder()
      strb append stringOfTypeConstraint(dg, cts.head)
      cts.tail.foreach {
        ct =>
          strb append " /\\ "
          strb append stringOfTypeConstraint(dg, ct)
      }
      strb.toString()
  }


  val desambiguatedLocalStringOfId : DGStringBuilder[NodeId] = {
    case (g, id) =>  desambiguatedLocalName(g, g getConcreteNode id)
  }

  implicit val stringOfAbstraction : DGStringBuilder[Abstraction] = {
    case (g, AccessAbstraction(nid, policy)) =>
      s"AccessAbstraction(${desambiguatedLocalStringOfId(g, nid)}, $policy)"
    case (g, ReadWriteAbstraction(srid, swid)) =>
      val rstr = srid map (desambiguatedLocalStringOfId(g,_))
      val wstr = swid map (desambiguatedLocalStringOfId(g,_))
      s"ReadWriteAbstraction(r = $rstr, w = $wstr)"
  }

  implicit def stringOfRecordable : DGStringBuilder[Recordable] = (dg, r) =>
    r match {
      case tf : Transformation =>
        if(Transformation.isAddRmOperation(tf.operation))
          s"${directAddRm(tf.direction)}(${stringOfOperation(dg, tf.operation)})"
        else stringOfOperation(dg, tf.operation)

      case MileStone => r.toString
      case Comment(msg) => s"***$msg***"
    }

  def print[K,C[_],V](s : CollectionValueMap[K, C, V], p : (K, V)  => String) : String = {
    val sb = new StringBuilder()
    s.iterator.foreach{ case (k, v) =>
      sb append "\t"
      sb append p(k, v)
      sb append "\n"
    }
    sb.toString()
  }
  implicit val stringOfEdgeMap : DGStringBuilder[EdgeMap] = {
    case (dg, EdgeMap ( userMap, usedMap, accessKindMap,
    contents, containers, superTypes, subTypes,
    typeMemberUses2typeUsesMap,
    typeUses2typeMemberUsesMap,
    typeConstraints,
    parameters, types, typedBy)) =>
      val builder = new StringBuilder(150)

//      builder.append("used -> user\n")
//      builder append print(userMap, (used : NodeId, user : NodeId) =>
//        s"$used - ${desambiguatedFullName(dg, used)} used by $user - ${desambiguatedFullName(dg, user)}")

//      builder.append("\nuser -> used\n")
//      builder append print(usedMap, (user : NodeId, used : NodeId) =>
//        s"$user - ${desambiguatedFullName(dg, user)} uses $used - ${desambiguatedFullName(dg, used)}")



//      builder.append("\ncontainer -> content\n")
//      builder append print(contents, (container : NodeId, content : NodeId) =>
//        s"$container - ${desambiguatedFullName(dg, container)} contains $content - ${desambiguatedFullName(dg, content)}")
//      builder.append("\ncontent -> container\n")
//      builder append containers.toList.map {
//        case (content, container) =>
//          s"$content - ${desambiguatedFullName(dg, content)} contained by $container - ${desambiguatedFullName(dg, container)}"
//      }.mkString("\t",",\n\t ","\n")
//
//      builder.append("\nmethod -> parameters\n")
//      builder append print(parameters, (container : NodeId, content : NodeId) =>
//        s"$container - ${desambiguatedFullName(dg, container)} contains $content - ${desambiguatedFullName(dg, content)}")
//
//
//      builder.append("\ntypes\n")
//      builder.append( types.toList map { case (nid, t) =>
//        s"$nid - ${desambiguatedFullName(dg, nid)} : ${stringOfType(dg, t)}"
//      } mkString("\t", "\n\t", "\n"))
//
      builder.append("\nsub -> super\n")
      builder append print(superTypes, (sub : NodeId, sup : Type) =>
        s"${desambiguatedFullName(dg, sub)} is a ${stringOfType(dg, sup)}")
//      builder.append("\nsuper -> sub\n\t")
//      builder.append(subTypes.toString)
//
//      val pToString : NodeIdP => String = {
//        case (p1, p2) => s"($p1 - ${desambiguatedFullName(dg, p1)}, $p2 - ${desambiguatedFullName(dg, p2)})"
//      }
//
//      builder append "\ntmUse -> tUse\n"
//      builder append print(typeMemberUses2typeUsesMap, (tmUse : NodeIdP, tUses : NodeIdP) =>
//        s"${pToString(tmUse)} -> ${pToString(tUses)}")
//
//      builder append "\ntUse -> tmUse\n"
//      builder append print(typeUses2typeMemberUsesMap, (tUse : NodeIdP, tmUse : NodeIdP) =>
//        s"${pToString(tUse)} -> ${pToString(tmUse)}")

//      builder.append("\ntypeConstraints\n")
//      val typeConstraintsSet =
//        typeConstraints.content.iterator.foldLeft(Set[TypeConstraint]()) {
//        case (s, (_, stc)) => s ++ stc
//      }
//      typeConstraintsSet foreach {
//        tc =>
//          builder append stringOfTypeConstraint(dg, tc)
//          builder append "\n"
//      }
//      builder append print(typeConstraints,
//        (k : NodeId, v : TypeConstraint) => stringOfTypeConstraint(dg, v))


      builder.toString()
  }


  def mkMapStringSortedByKey[A,B](m : Map[A,B])(implicit ord: Ordering[A]) : String = {
    m.toList.sortBy(_._1).mkString("\t[",",\n\t ","]\n")
  }

  def mkMapStringSortedByFullName(g : DependencyGraph, m : Map[NodeId,DGNode]) : String = {
    m.toList.map{ case (id, n) =>  ((g, id).shows(desambiguatedFullName) +" - " + n.kind , id) }.
      sortBy(_._1).mkString("\t[",",\n\t ","]\n")
  }



  implicit val stringOfNodeIndex : DGStringBuilder[NodeIndex] = {
    case (g, NodeIndex(_, cNodes, removedCnodes,
    vNodes, removedVnodes,
    cNodes2vNodes,
    roles)) =>
      s"Concrete Nodes : (${cNodes.size})" +
        mkMapStringSortedByFullName(g, cNodes) +
        "Removed Concrete Nodes : " +
        mkMapStringSortedByFullName(g, removedCnodes) +
        "Virtual Nodes : " +
        mkMapStringSortedByFullName(g, vNodes) +
        "Removed Virtual Nodes : " +
        mkMapStringSortedByFullName(g, removedVnodes) +
        "CN -> VN : " +
        cNodes2vNodes.mkString("\t[",",\n\t ","]\n") +
        "Roles : " +
        roles.mkString("\t[",",\n\t ","]\n")
  }

  implicit val stringOfAbstractionMap : DGStringBuilder[AbstractionMap] = {
    case (g, m) =>
      val sb  = new StringBuilder()
      sb append "Abstraction Map : {\n"
      m.toList foreach { case (id, abs) =>
        sb append desambiguatedLocalStringOfId(g, id)
        sb append " :\n"
        sb append (abs map (stringOfAbstraction(g, _)) mkString("\t[", ",\n\t" ,"]\n"))
      }
      sb append "}\n"
      sb.toString()
  }

  implicit class DGShowOp[A](val p : (DependencyGraph, A)) extends AnyVal {
    def shows(implicit cb : DGStringBuilder[A]) : String = cb(p._1, p._2)

    def println(implicit cb : DGStringBuilder[A]) : Unit = System.out.println(shows)
  }
}
