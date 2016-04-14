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

import puck.graph.constraints.ShowConstraints
import puck.graph.transformations._

import scalaz.{Cord, Show}


object ShowDG extends ShowConstraints{

  implicit def showFromToString[A] : Show[A] = Show.showFromToString[A]



  def tailRecTypeCord
  (dg : DependencyGraph,
   sep : List[String], end : List[String],
   builder : StringBuilder, lt : List[List[Type]]) : String =
    if (lt.isEmpty) builder.toString()
    else lt.head match {
      case Nil => tailRecTypeCord(dg, sep.tail, end.tail, builder append end.head, lt.tail)
      case NamedType(nid) :: tl =>
        val sep0 =
          if(tl.nonEmpty) sep.head
          else ""
        tailRecTypeCord(dg, sep, end,
          builder.append(dg.getNode(nid).name(dg) + sep0),
          tl :: lt.tail )
      case Tuple(types) :: tl =>
        tailRecTypeCord(dg, "," :: sep, (")" + sep.head) :: end,
          builder append "(", types :: tl :: lt.tail )
      case Arrow(in, out) :: tl =>
        tailRecTypeCord(dg, " -> " :: sep, "" :: end, builder, List(in, out) :: tl :: lt.tail )

      case ParameterizedType(genId, targs) :: tl =>
        val genName = dg.getNode(genId).name(dg)
        tailRecTypeCord(dg, "," :: sep, (">" + sep.head) :: end,
          builder append s"$genName<", targs :: tl :: lt.tail )

      case Covariant(t) :: tl =>
        tailRecTypeCord(dg,  sep,  end,
          builder append s"+", (t :: tl) :: lt.tail )
      case Contravariant(t) :: tl =>
        tailRecTypeCord(dg,  sep,  end,
          builder append s"-", (t :: tl) :: lt.tail )

    }

  implicit def typeCord : CordBuilder[Type] = (dg, t) =>
    tailRecTypeCord(dg, List(""), List(""), new StringBuilder, List(List(t)))

  implicit def typeHolderCord : CordBuilder[Option[Type]] = (dg, th) => th match {
    case None => ""
    case Some(t) => Cord(" : ", typeCord(dg, t))
  }

  implicit def nodeIdCord : CordBuilder[NodeId] =
    (dg, nid) => nodeCord(dg, dg.getNode(nid))

  implicit def nodeIdPCord : CordBuilder[NodeIdP] =
    {case (dg, (nid1, nid2)) => Cord("Edge(", nodeIdCord(dg,nid1), ", ", nodeIdCord(dg,nid2), ")")}


  implicit def nodeCord : CordBuilder[DGNode] = (dg, n) =>
    n match {
      case n : ConcreteNode =>
        val name =
          if(n.kind.kindType == ValueDef)
            dg.container(n.id) map {
              dg.getConcreteNode(_).name + DependencyGraph.scopeSeparator + n.name
            } getOrElse "OrphanDefinition"
          else n.name
        Cord(s"${n.id} - ${n.kind} $name", typeHolderCord(dg, dg.styp(n.id)))
      case vn : VirtualNode => Cord(s"${vn.id} - ${vn.name(dg)}")
    }

  def nodeNameTypCord : CordBuilder[DGNode] =
    (dg, n) => n match {
      case cn : ConcreteNode => Cord(cn.name , typeHolderCord(dg, dg.styp(cn.id)))
      case _ => n.name(dg)
    }


  def desambiguatedLocalName : CordBuilder[DGNode] =  (g, n) => {
    n.kind.kindType match {
      case StaticValueDecl
           | InstanceValueDecl
           | TypeConstructor =>
        g.structuredType(n.id) match {
          case Some(Arrow(in, _)) =>
            n.name + typeCord(g, in)
          case _ => n.name
        }
      case _ =>  n.name
    }
  }
  def desambiguatedFullName : CordBuilder[NodeId] = (g, n) => {
    val ss = DependencyGraph.scopeSeparator
    def aux(nid: NodeId, accu: String): Cord = {
      val n = g.getNode(nid)
      g.container(n.id) match {
        case None if n.id == g.rootId =>
          if(accu.isEmpty) Cord(g.root.name)
          else Cord(accu.substring(1))
        case None => Cord(DependencyGraph.unrootedStringId, ss, n.name, accu)
        case Some(pid) =>
          aux(pid, ss + desambiguatedLocalName(g, n) + accu)
      }
    }
    aux(n, "")
  }


  def fullNameEdgeCord : CordBuilder[DGEdge] =  (g, e) =>
    Cord(s"${e.kind}(${e.source} - ${g.fullName(e.source)}, ${e.target} - ${g.fullName(e.target)})")


  implicit def edgeCord : CordBuilder[DGEdge] =  (dg, e) =>
    Cord(e.kind.toString, "( " + nodeIdCord(dg, e.source), ", ", nodeIdCord(dg, e.target), ")")
  implicit def brCord : CordBuilder[(DGEdge,DGEdge)] =
  {case (dg, (u1, u2)) => Cord("Edge(", edgeCord(dg,u1), ", ", edgeCord(dg,u2), ")")}


  implicit def extremityCord : CordBuilder[Extremity] =
    (dg, e) => Cord(e.productPrefix, "(", nodeCord(dg, dg.getNode(e.node)), ")")

  implicit def transfoTargetCord : CordBuilder[Operation] = (dg, tgt) =>
    tgt match {
      case CNode(n) => nodeCord(dg, n)
      case VNode(n) =>nodeCord(dg, n)
      case Edge(edge) => edgeCord(dg, edge)
      case RedirectionOp(edge, Target(newTgt))
        if dg.kindType(edge.target) == TypeDecl =>

        val typed = nodeIdCord(dg, edge.source)
        val oldType = nodeIdCord(dg, edge.target)
        val newType =  nodeIdCord(dg, newTgt)
        Cord("TypeChange(", typed ,",", oldType, ",", newType ,")")

      case RedirectionOp(edge, exty) =>
        val ecord = edgeCord(dg, edge)
        val xcord = extremityCord(dg, exty)
        Cord(tgt.productPrefix, "(", ecord ,",", xcord ,")")
      case TypeBinding((n1,n2), (n3,n4)) =>
        Cord(tgt.productPrefix, "(Uses(",  nodeIdCord(dg, n1), ", ", nodeIdCord(dg,n2),
          "),Uses(", nodeIdCord(dg, n3), ", ", nodeIdCord(dg,n4) ,"))")

      case AType(typed, t) =>
        val typedCord = nodeIdCord(dg, typed)
        val tcord : Cord = typeCord(dg, t)
        Cord("SetType(", typedCord, ", ", tcord,")")

      case ChangeTypeBindingOp((tUse, tmUse), exty) =>
        val tUseCord = nodeIdPCord(dg, tUse)
        val tmUseCord = nodeIdPCord(dg, tmUse)
        Cord("ChangeTypeBinding((", tUseCord, ", ", tmUseCord,"),", exty.productPrefix,
          "(", nodeIdPCord(dg, exty.edge) ,")")
      case _ => tgt.toString
    }

  val directAddRm : Direction => String = {
    case Regular => "Add"
    case Reverse => "Remove"
  }


  implicit def abstractionCordBuilder : CordBuilder[Abstraction] = (dg, a) =>
    a match {
      case AccessAbstraction(nid, policy) =>
        Cord("AccessAbstraction(", dg.getNode(nid).name , ", ", policy.toString,")")
      case ReadWriteAbstraction(rid, wid) =>
        val n1 = rid map (dg.getNode(_).name) toString ()
        val n2 = wid map (dg.getNode(_).name) toString ()
        Cord("ReadWriteAbstraction(", n1 , ", ", n2,")")
    }

  implicit def transformationtCord : CordBuilder[Recordable] = (dg, r) =>
    r match {
      case tf : Transformation =>
        if(Transformation.isAddRmOperation(tf.operation))
          Cord(directAddRm(tf.direction), "(", transfoTargetCord(dg, tf.operation),")")
        else transfoTargetCord(dg, tf.operation)

      case MileStone => Cord(r.toString)
      case Comment(msg) => Cord("***", msg, "***")
    }


  implicit class DGShowOp[A](val p : (DependencyGraph, A)) extends AnyVal {
    def shows(implicit cb : CordBuilder[A]) : String =
      cb(p._1, p._2).toString()

    def println(implicit cb : CordBuilder[A]) : Unit = System.out.println(shows)
  }
}
