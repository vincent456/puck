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
package comparison

import puck.PuckError
import puck.graph.transformations._
import puck.search.SearchControl
import puck.util.PuckLogger
import scalaz.Scalaz._
import scalaz._

object RecordingComparator{

  type ResMap = Map[NodeId, Either[NodeKind, NodeId]]
  def ResMap() = Map[NodeId, Either[NodeKind, NodeId]]()
  type NodesToMap = Map[NodeKind, Seq[NodeId]]
  def NodesToMap() = Map[NodeKind, Seq[NodeId]]()
  type AttribT = (NodeId, ResMap, NodesToMap)

  type Compared = (ResMap, NodesToMap, Seq[Transformation], Seq[Transformation])

  def revAppend[T] (heads : Seq[T], tails : Seq[T]) : Seq[T] = {
     def aux(hds: Seq[T], tls : Seq[T]) : Seq[T] =
      if(hds.isEmpty) tls
      else aux(hds.tail, hds.head +: tls)

     aux(heads, tails)
  }

  def removeFirst[T](l : Seq[T], pred : T => Boolean) : Option[Seq[T]] = {
    def aux(l1 : Seq[T], acc : Seq[T]) : Option[Seq[T]] =
      if(l1.isEmpty) None
      else if(pred(l1.head)) Some(revAppend(acc, l1.tail))
      else aux(l1.tail, l1.head +: acc)

    aux(l, Seq[T]())
  }

  def removeFirst(l : Seq[Transformation],
                  op : Direction,
                  tgt : Operation) :  Option[Seq[Transformation]] =
    removeFirst(l, {(t : Transformation) => t.direction == op && t.operation == tgt})

  def attribNode
  (node : NodeId,
   currentResMap : ResMap,
   nodesToMap : NodesToMap) :
  Seq[LoggedTry[(ResMap, NodesToMap)]] = {

    currentResMap/*(node)*/ .getOrElse(node, Right(node)) match {
      case Right(n) => //already attributed
        Seq(LoggedSuccess((currentResMap, nodesToMap)))

      case Left(kind) =>
        nodesToMap.getOrElse(kind, Seq()) match {
          case Seq() => Seq(LoggedError("NoSolution"))
          case l =>
            l.toStream  map {
              mapped =>
                val newResMap : ResMap = currentResMap + (node -> Right(mapped))
                val newNodesToMap = nodesToMap + (kind -> l.filter(_ == mapped))
                LoggedSuccess((newResMap, newNodesToMap))
            }

        }
    }
  }

  def attribNodes
  (nodes : Seq[NodeId],
   currentResMap : ResMap,
   nodesToMap : NodesToMap) :
  Seq[LoggedTry[(ResMap, NodesToMap)]] = {

    def aux
    (nodes : Seq[NodeId],
     acc : Seq[LoggedTry[(ResMap, NodesToMap)]]) :
    Seq[LoggedTry[(ResMap, NodesToMap)]] = nodes match {
      case Seq() => acc
      case n +: tl =>
        aux(tl,
          acc.flatMap {
            lt =>
              lt.value match {
                case \/-((currResMap, nodesToMap0)) =>
                  attribNode(n, currResMap, nodesToMap0)
                case -\/(e) => Seq(LoggedError(lt.log, e))
              }
          })
    }
    aux(nodes, Seq(LoggedSuccess((currentResMap, nodesToMap))))
  }


  def getMapping(nodes : Seq[NodeId],
                 currentResMap : ResMap) : Option[Seq[NodeId]] =
    nodes.reverse.foldLeft(Seq[NodeId]().some){
      case (None, _) => none
      case (Some(acc),n) => currentResMap.getOrElse(n, Right(n)) match {
        case Left(_) => None
        case Right(mapped) => Some(mapped +: acc)
      }
    }

  val nextStates : Compared => Seq[LoggedTry[Compared]] = {
    case (_, _, Seq(), Seq()) => Seq()
    case (resMap, nodesToMap, t1 +: ts1, ts2) =>
      def removeFirstAndCompareNext(tgt : Operation) : Seq[LoggedTry[Compared]]  =
        removeFirst(ts2, t1.direction, tgt) match {
          case None => Seq(LoggedError("WrongMapping"))
          //              println("Failure on mapping : ")
          //              println(map.mkString("\t", "\n\t", "\n"))
          //              println(ts1.head + " mapped as ")
          //              println(Transformation(ts1.head.direction, tgt) + "cannot be found in :")
          //              println(ts2.mkString("\t", "\n\t", "\n"))
          //              println("**************************************")
          case Some(newTs2) =>
            nextStates((resMap, nodesToMap, ts1, newTs2))
        }

      t1.operation match {
        case Edge(e) =>
          getMapping(Seq(e.source, e.target), resMap) match {
            case Some(Seq(src, tgt)) =>
              removeFirstAndCompareNext(Edge(e.copy(src, tgt)))
            case None =>
              attribNodes(Seq(e.source, e.target), resMap, nodesToMap) map ( _ map {
                case (rm, ntm) => (rm, ntm, t1 +: ts1, ts2)
              })

          }

        case RedirectionOp(e, extremity) =>
          getMapping(Seq(e.source, e.target, extremity.node), resMap) match {
            case Some(Seq(src, tgt, newExtyNode)) =>
              removeFirstAndCompareNext(
                RedirectionOp(e.kind(src, tgt),
                  extremity.create(newExtyNode)))
            case None =>
              attribNodes(Seq(e.source, e.target, extremity.node), resMap, nodesToMap) map ( _ map {
                case (rm, ntm) => (rm, ntm, t1 +: ts1, ts2)
              })
          }


        //case TTNode(_) => throw new Error("should not happen !!")
        //removing the dependendency and abstraction of the comparison
        // they are used to compute the change on the graph, its the change themselves we want to compare
        // removed in NodeMappingInitialState.normalizeNodeTransfos
        case _ : AType // TODO see if need to be compared
             | _ : TypeBinding
             | _ : ChangeTypeBindingOp
             | _ : TypeConstraintOp
             | _ : AbstractionOp
             | _ : VNode
             | _ : CNode
             | _ : RenameOp
             | _ : RoleChange
             | _ : Comment => throw new Error("should not happen !!")

      }
  }
}

object NoSolution extends PuckError("No solution")
object WrongMapping extends PuckError("Wrong mapping")

import RecordingComparator._

class RecordingComparatorControl
( initialRecord : Seq[Transformation],
  graph1 : DependencyGraph,
  graph2 : DependencyGraph,
  logger : PuckLogger )
  extends SearchControl[Compared]{

  def initialState : Compared = RecordingComparatorInitialState(initialRecord, graph1, graph2, logger)
  def nextStates(t: Compared): Seq[LoggedTry[Compared]] =
    RecordingComparator.nextStates(t)
}



