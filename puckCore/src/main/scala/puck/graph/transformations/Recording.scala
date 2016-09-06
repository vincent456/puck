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
package transformations

import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}

import puck.graph.comparison.Mapping
import puck.util.PuckLogger


object Recording {
  def apply() = Seq[Recordable]()

  def write(oos : ObjectOutputStream,
            r: Recording): Unit = {
    val i : Int = r.size
    oos.writeInt(i)
    r.reverse foreach { oos.writeObject }
  }

  def read(ois : ObjectInputStream) : Recording = {
    val recSize = ois.readInt()
    var rec = Recording()
    for (i <- Range(0, recSize)) {
      rec = ois.readObject().asInstanceOf[Recordable] +: rec
    }
    rec
  }

  def write(fileName : String,
            map : Map[String, NodeId],
            g: DependencyGraph): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fileName))
    oos.writeObject(map)

    val numIds = g.numNodes + g.numRemovedNodes
    oos.writeInt(numIds)

    write(oos, g.recording)
  }

  type NumIds = Int
  def read(fileName : String) : (Map[String, NodeId], NumIds, Recording) = {
    val ois = new ObjectInputStream(new FileInputStream(fileName))
    val map = ois.readObject().asInstanceOf[Map[String, NodeId]]
    val numIds = ois.readInt()
    val rec = read(ois)
    (map, numIds, rec)
  }

  case class LoadError(msg: String, map : Map[String, NodeId]) extends Throwable
  def load(fileName : String, map :  Map[String, NodeId])
          (implicit logger : PuckLogger): Recording = {
    val (recMap, numIds, r) = read(fileName)
    try mapNodes(r, createMapping(recMap, map, numIds))
    catch {
      case e : Error =>
        throw LoadError(e.getMessage, recMap)
      case e : Exception =>
        throw LoadError(e.getMessage, recMap)
    }
  }



  def createMapping(recordIds : Map[String, NodeId],
                    newIds : Map[String, NodeId],
                    totalIds : Int )
                   (implicit logger : PuckLogger): NodeId => NodeId = {
    //assert(recordIds.size == newIds.size, "Map should be of same size")
    if(recordIds.size != newIds.size) {
      val recordedIds = recordIds.keys.toSet
      val currentGraphIds = newIds.keys.toSet
      val diff1 = recordedIds -- currentGraphIds
      val diff2 = currentGraphIds -- recordedIds

      logger writeln ("fullname recorded : " + recordIds.size)
      logger writeln "recordedIds -- currentGraphIds = "
      logger writeln diff1
      logger writeln " currentGraphIds -- recordedIds = "
      logger writeln diff2
      error("mapping creation error recordIds.size != newIds.size")

    }
    assert(totalIds >= recordIds.size)

    if(recordIds == newIds) identity
    else {
      val m1 = Mapping.create(recordIds, newIds)

      Range(recordIds.size, totalIds).foldLeft(m1) { case (m, id) =>
        m + (id -> id)
      }.apply
    }
  }

  def mapNodes
  (rec : Recording,
   mappin : NodeId => NodeId) : Recording = {


    rec.mapTransformation {
        case Transformation(direction, op) =>
          Transformation(direction, Mapping.operation(mappin, op))
      }
  }

  implicit class RecordingOps(val record : Recording) extends AnyVal {

    def redo(g : DependencyGraph) : DependencyGraph =
      record.reverse.foldLeft(g)((g0, t) => t.redo(g0))

    def mapTransformation(f : Transformation => Transformation) : Recording =
      record map {
        case t : Transformation => f(t)
        case r => r
      }

    def comment(msg : String) : Recording =
      Comment(msg) +: record

    def mileStone : Recording = MileStone +: record

    def splitAtLastMilestone : (Recording, Recording) = {
      def f(r0 : Recording, acc : Recording) :  (Recording, Recording) = r0 match {
        case Nil => (Nil, acc.reverse)
        case MileStone +: tl => (tl, acc.reverse)
        case hd +: tl => f(tl, hd +: acc)
      }

      f(record, List())
    }

    def splitAtMilestones : Seq[Recording] = {
      def aux(r : Recording, acc : List[Recording]) : Seq[Recording] =
        r.splitAtLastMilestone match {
          case (Nil, rec) => rec :: acc
          case (remaining, rec) => aux(remaining, rec :: acc)

        }
      aux(record, List())
    }

    def subRecordFromLastMilestone : Recording =
      splitAtLastMilestone._2




    def involveNodes : Seq[NodeId] = record.foldLeft(Seq[NodeId]()){
      case (acc, Transformation(_, op)) =>
        Operation.involvedNodes(op) ++: acc
      case (acc, _) => acc

    }

    def concretize(vNodeId : NodeId, cNodeId : NodeId) : Recording = {
      val r2 = record.filterNot {case Transformation(Regular, VNode(vn)) => vn.id == vNodeId
      case _ => false}
      Recording.mapNodes(r2, {
        id =>
          if(id == vNodeId) cNodeId
          else id
      })
    }

    def commentsSinceLastMileStone : Seq[String] = {
      def aux(r : Recording, acc : Seq[String] = Seq()) : Seq[String] =
        if(r.isEmpty) acc
        else r.head match {
          case MileStone => acc
          case Comment(msg) => aux(r.tail, msg +: acc)
          case _ => aux(r.tail, acc)
        }

      aux(record)
    }


    def addConcreteNode(n : ConcreteNode) : Recording =
      Transformation(Regular, CNode(n)) +: record

    def addVirtualNode(n : VirtualNode) : Recording =
      Transformation(Regular, VNode(n)) +: record


    def changeNodeName(nid : NodeId, oldName : String, newName : String) : Recording =
      Transformation(Regular, RenameOp(nid, oldName, newName)) +: record

    def removeConcreteNode(n : ConcreteNode) : Recording =
      Transformation(Reverse, CNode(n)) +: record

    def removeVirtualNode(n : VirtualNode) : Recording =
      Transformation(Reverse, VNode(n)) +: record

    def addEdge(edge : DGEdge) : Recording =
      Transformation(Regular, Edge(edge)) +: record

    def removeEdge(edge : DGEdge) : Recording=
      Transformation(Reverse, Edge(edge)) +: record

    def changeEdgeTarget(edge : DGEdge, newTarget : NodeId, withMerge : Boolean) : Recording = {
      val red = if(withMerge) new RedirectionWithMerge(edge, Target(newTarget))
      else RedirectionOp(edge, Target(newTarget))
      Transformation(Regular, red) +: record
    }

    def changeEdgeSource(edge : DGEdge, newTarget : NodeId, withMerge : Boolean) : Recording = {
      val red = if(withMerge) new RedirectionWithMerge(edge, Source(newTarget))
      else RedirectionOp(edge, Source(newTarget))
      Transformation(Regular, red) +: record
    }
    def addType( typed : NodeId, t : Type) : Recording =
      Transformation(Regular, AType(typed, t)) +: record

    def removeType( typed : NodeId, t : Type) : Recording =
      Transformation(Reverse, AType(typed, t)) +: record

    def addRoleChange( typed : NodeId,
                       oldRole: Option[Role],
                       newRole : Option[Role]) : Recording =
      Transformation(Regular, RoleChange(typed, oldRole, newRole)) +: record

    def addAbstraction(impl : NodeId, abs : Abstraction) : Recording =
      Transformation(Regular, AbstractionOp(impl, abs)) +: record

    def removeAbstraction(impl : NodeId, abs : Abstraction) : Recording =
      Transformation(Reverse, AbstractionOp(impl, abs)) +: record

    def addTypeBinding(typeUse : NodeIdP,
                       typeMemberUse :  NodeIdP) : Recording =
      Transformation(Regular, TypeBinding(typeUse, typeMemberUse)) +: record

    def removeTypeBinding(typeUse : NodeIdP,
                          typeMemberUse :  NodeIdP) : Recording =
      Transformation(Reverse, TypeBinding(typeUse, typeMemberUse)) +: record

    def addTypeConstraint(constraint :  TypeConstraint) : Recording =
      Transformation(Regular, TypeConstraintOp(constraint)) +: record

    def removeTypeConstraint(constraint :  TypeConstraint) : Recording =
      Transformation(Reverse, TypeConstraintOp(constraint)) +: record

    def changeTypeUseOfTypeMemberUse
    ( oldTypeUse : NodeIdP,
      newTypeUse : NodeIdP,
      typeMemberUse :  NodeIdP) : Recording =
      Transformation(Regular, ChangeTypeBindingOp((oldTypeUse, typeMemberUse),
        TypeUse(newTypeUse))) +: record

    def changeTypeMemberUseOfTypeUse
    ( oldTypeMemberUse : NodeIdP,
      newTypeMemberUse : NodeIdP,
      typeUse :  NodeIdP) : Recording =
      Transformation(Regular, ChangeTypeBindingOp((typeUse, oldTypeMemberUse),
        InstanceValueUse(newTypeMemberUse))) +: record



  }

}







