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

package puck.graph.comparison

import java.util.NoSuchElementException

import puck.graph._



object Equality {

  def equalsCVM[C[_], V]
  ( mappin : V => V)
  ( cvm1 : CollectionValueMap[V, C, V],
    cvm2 : CollectionValueMap[V, C, V]) : Boolean =
    equalsCVM[V, C, V](mappin, mappin)(cvm1, cvm2)


  def equalsCVM[K, C[_], V]
  ( mappinKey : K => K,
    mappinValue : V => V)
  ( cvm1 : CollectionValueMap[K, C, V],
    cvm2 : CollectionValueMap[K, C, V]) : Boolean =
    cvm1.content.size == cvm2.content.size &&
      cvm1.content.forall {
        case ((k1, vs1)) =>
          val vs2 = cvm2.content(mappinKey(k1))
          cvm1.handler.map(vs1, mappinValue) == vs2
      }

}

object EqualityWithDebugPrint {


  import ShowDG.{DGStringBuilder, DGShowOp}

  def equalsCVM[C[_], V]
  ( mappin : V => V)
  ( cvm1 : CollectionValueMap[V, C, V],
    cvm2 : CollectionValueMap[V, C, V])
  (implicit  dgp : (DependencyGraph, DependencyGraph),
   sb : DGStringBuilder[V]): Boolean =
    equalsCVM[V, C, V](mappin, mappin)(cvm1, cvm2)(dgp,sb,sb)


  def equalsCVM[K, C[_], V]
  ( mappinKey : K => K,
    mappinValue : V => V)
  ( cvm1 : CollectionValueMap[K, C, V],
    cvm2 : CollectionValueMap[K, C, V])
  (implicit  dgp : (DependencyGraph, DependencyGraph),
   ksb : DGStringBuilder[K],
   vsb : DGStringBuilder[V]): Boolean = {
    val (g1, g2) = dgp
    def msg(g: DependencyGraph)(k: K, vs: C[V]): String = {
      val kmsg = (g, k).shows
      cvm1.handler.toList(vs) map (v => kmsg + " " +(g, v).shows) mkString "\n"

    }

    if (cvm1.content.size != cvm2.content.size) {
      val mappedCvm1 = cvm1.toList map {
        case (k, vs) => (mappinKey(k), cvm1.handler.map(vs, mappinValue))
      }
      val diff1 = mappedCvm1 diff cvm2.toList
      val diff2 = cvm2.toList diff mappedCvm1
      println("diff1 = \n" + (diff1 map (msg(g1) _).tupled mkString "\n"))
      println("-----")
      println("diff2 = \n" + (diff2 map (msg(g2) _).tupled mkString "\n"))
      false
    }
    else
      cvm1.content.forall {
        case ((k1, vs1)) =>

          val vs2 = try cvm2.content(mappinKey(k1))
          catch {
            case e : NoSuchElementException =>
              val k2 = mappinKey(k1)
              println(cvm1.content mkString ",")
              println(cvm2.content mkString ",")
              error( (g1, k1).shows + " mapped as " + (g2, k2).shows + " not found ")
          }
          if (cvm1.handler.map(vs1, mappinValue) != vs2) {
            error(msg(g1)(k1, vs1) + " <-^->" + msg(g2)(mappinKey(k1), vs2))
          }
          true

      }
  }

}

object Mapping {

  def create
  ( g1 : DependencyGraph,
    g2 : DependencyGraph
  ) : Map[NodeId, NodeId] =
    create(nameIndex(g1), nameIndex(g2))

  def nameIndex(g : DependencyGraph) : Map[String, NodeId] = {
    import ShowDG._
    // g.nodesId map ( id => ((g, id).shows(sigFullName), id) ) toMap
    (g.nodesId map ( id => ((g, id).shows(desambiguatedFullName), id) )).foldLeft(Map[String, NodeId]()){
      case (m, (k,id)) =>
        if(m contains k) {
          val id0 = m(k)
          val n = g.getNode(id)
          val n0 = g.getNode(id0)
          error(s"$k already in map !!!! oldVal = $n0 newVal = $n" )
        }
        else m + (k -> id)
    }
  }





  def create
  ( m1 : Map[String, NodeId],
    m2 : Map[String, NodeId]
  ) : Map[NodeId, NodeId] =
    swap(m1) mapValues m2.apply
  //{
  //   m1.foldLeft(Map[NodeId, NodeId]()){
  //     case (m, (name, nid1)) =>
  //       m2 get name match {
  //         case None => throw new PuckError(s"$name not found while building mapping")
  //         case Some(nid2) => m + (nid1 -> nid2)
  //       }
  //    }
  //   }


  def swap[A,B]( m : Map[A, B]) : Map[B, A] =
    m.toList.map(_.swap).toMap


  def mapCVM[C[_], V]
  ( mappin : V => V, cvm : CollectionValueMap[V, C, V]) ={
    val l : List[(V, C[V])]= cvm.toList map {
      case (k, v) =>
        (mappin(k), cvm.handler.map(v, mappin))
    }

    new CollectionValueMap(l.toMap, cvm.handler)
  }

  def mapType(mappin : NodeId => NodeId): Type => Type = {
    case NamedType(id) => NamedType(mappin(id))
    case Tuple(lt) => Tuple(lt.map(mapType(mappin)))
    case Arrow(i, o) => Arrow(mapType(mappin)(i), mapType(mappin)(o))
    case ParameterizedType(gid, targs) =>
      ParameterizedType(mappin(gid), targs.map(mapType(mappin)))

    case Covariant(t) => Covariant(mapType(mappin)(t))
    case Contravariant(t) => Contravariant(mapType(mappin)(t))
  }

  def equalsMap[V]
  ( mappin : V => V)
  ( cvm1 : Map[V, V],
    cvm2 : Map[V, V]) : Boolean =
    cvm1.size == cvm2.size &&
      cvm1.forall {
        case ((k1, v1)) =>
          val v2 = cvm2(mappin(k1))
          mappin(v1) == v2
      }


//  import ShowDG._
//  import EqualityWithDebugPrint._
  import Equality._

  def equals
  ( g1 : DependencyGraph,
    g2 : DependencyGraph
  ) : Boolean = {
    assert(g1.virtualNodes.isEmpty)
    assert(g2.virtualNodes.isEmpty)

    g1.nodesId.size >= g2.nodesId.size && {
    val mappinG1toG2 : Map[NodeId, NodeId] = create(g1, g2)
    implicit val gp = (g1, g2)

      val mappinNodeIdP : NodeIdP => NodeIdP = {
        case (n1, n2) => (mappinG1toG2(n1), mappinG1toG2(n2))
      }

      val equalsNodes = g1.concreteNodes.forall{
        g1n =>
          try {
            val g2Id = mappinG1toG2(g1n.id)
            val g2n = g2.getConcreteNode(g2Id).copy(id = g1n.id)
            g1n == g2n
          } catch {
            case _ : NoSuchElementException =>
              g1.nodesIndex.getConcreteNodeWithStatus(g1n.id) match {
                case (_, Removed) => true
                case _ => false
              }
          }

      }

      lazy val equalsUses =
        equalsCVM(mappinG1toG2)(g1.edges.userMap, g2.edges.userMap)

      lazy val equalTypes : Boolean =
        g1.edges.types.forall {
          case ((id1, t1)) =>
            val t2 = g2.edges.types(mappinG1toG2(id1))
            mapType(mappinG1toG2)(t1) == t2
        }
      lazy val equalsUsesAccessKind = g1.edges.accessKindMap.forall{
        case (k, v) =>
          g2.edges.accessKindMap(mappinNodeIdP(k)) == v
      }

      lazy val equalsContains1 =
        equalsCVM(mappinG1toG2)(g1.edges.contents, g2.edges.contents)
      lazy val equalsContains2 =
        equalsCVM(mappinG1toG2)(g1.edges.parameters, g2.edges.parameters)

      lazy val equalsIsa =
        equalsCVM(mappinG1toG2)(g1.edges.superTypes, g2.edges.superTypes)


      lazy val equalsBR =
        equalsCVM(mappinNodeIdP)(g1.edges.typeMemberUses2typeUsesMap,
          g2.edges.typeMemberUses2typeUsesMap)
      //      lazy val equalsTD2 =
      //        equalsCVM(mappinNodeIdP)(g1.edges.typeUses2typeMemberUsesMap,
      //          g2.edges.typeUses2typeMemberUsesMap)

      def mappinTUC(tuc : TypeUseConstraint) : TypeUseConstraint =
        tuc.copyWith(user = mappinG1toG2(tuc.constrainedUser),
          used = mappinG1toG2(tuc.constrainedType))


      lazy val equalsTypeUseConstraints =
        equalsCVM(mappinNodeIdP, mappinTUC)(g1.edges.typeUsesConstraints,
          g2.edges.typeUsesConstraints)


//      println("###############################")
//      println("equalsNodes = " + equalsNodes)
//      println("equalsUses = " + equalsUses)
//      println("equalTypes = " + equalTypes)
//      println("equalsUsesAccessKind = " + equalsUsesAccessKind)
//      println("equalsContains1 = " + equalsContains1)
//      println("equalsContains2 = " + equalsContains2)
//      println("equalsIsa = " + equalsIsa)
//      println("equalsBR = " + equalsBR)
//      println("equalsTypeUseConstraints = " + equalsTypeUseConstraints)

      equalsNodes && equalsUses && equalTypes && equalsUsesAccessKind &&
        equalsContains1 && equalsContains2 &&
        equalsIsa && equalsBR && equalsTypeUseConstraints

    }


  }


}
