package puck.graph.transformations

import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}

import puck.graph._
import puck.graph.constraints.AbstractionPolicy

/**
 * Created by lorilan on 27/10/14.
 */


object Recording {
  def apply() = new Recording(Seq())
  def write(fileName : String, map : Map[String, NodeId], r : Recording): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(fileName))
    oos.writeObject(map)

    val i : Int = r.size
    oos.writeInt(i)
    r foreach oos.writeObject
  }

  def read(fileName : String) : (Map[String, NodeId], Recording) = {
    val ois = new ObjectInputStream(new FileInputStream(fileName))
    val map = ois.readObject().asInstanceOf[Map[String, NodeId]]
    val recSize = ois.readInt()
    var rec = Recording()
    for(i <- Range(0, recSize)){
      rec =  ois.readObject().asInstanceOf[Transformation] +: rec
    }
    (map, rec)
  }

  def load(fileName : String, map :  Map[String, NodeId]) : Recording = {
    val (m, r) = read(fileName)
    mapNodes(r, m, map)
  }

  private def swap[A,B]( m : Map[A, B]) : Map[B, A] =
    m.toList.map {case (a,b) => (b,a)}.toMap

  def mapNodes
  (rec : Recording,
   currentIds : Map[String, NodeId],
   newIds : Map[String, NodeId]) : Recording = {
    val mappin : NodeId => NodeId =
      swap(currentIds).mapValues(newIds.apply).apply


    def mappingOnType: Type => Type = {
      case nt : NamedType => nt.copy(mappin(nt.id))
      case nt : Tuple => nt.copy(nt.types.map(mappingOnType))
      case nt : Arrow =>
        val i = nt.input
        val o = nt.output
        nt.copy(input = mappingOnType(i), output = mappingOnType(o))
    }


    def mappingOnTransfoTgt : TransformationTarget => TransformationTarget = {
      case TTCNode(n) =>
        val n2 = n.copy(id = mappin(n.id))
        TTCNode(n2.copy(styp = n.styp.map(mappingOnType)))
      case TTVNode(n) =>
        val newId = mappin(n.id)
        TTVNode(n.copy(id = newId))

      case TTEdge(e) =>
        TTEdge(e.copy(source = mappin(e.source), target = mappin(e.target)))

      case tgt : TTRedirection =>
        val newEdge =
          mappingOnTransfoTgt(TTEdge(tgt.edge)).asInstanceOf[TTEdge].edge
        val extyId = tgt.extremity.node
        val newExty = tgt.extremity.create(mappin(extyId))
        tgt.copy(edge = newEdge, extremity = newExty)
      case TTAbstraction(impl, abs, p) =>
        TTAbstraction(mappin(impl), mappin(abs), p)

      case TTTypeRedirection(typed, styp, oldUsed, newUsed) =>
        TTTypeRedirection(mappin(typed), styp map mappingOnType,
          mappin(oldUsed), mappin(newUsed))

    }

    rec.mapTransformation {
        case Transformation(op, target) =>
          Transformation(op, mappingOnTransfoTgt(target))
      }
  }

}

class Recording
(private [this] val record : Seq[Transformation]) extends Iterable[Transformation] {

  type NIdT = NodeId
  type EdgeT = DGEdge
  type RecT = Recording
  def apply() = record

  private def mapTransformation(f : Transformation => Transformation) : Recording =
    new Recording (record map f)


  def +:(r : Transformation) : Recording =
    new Recording(r +: record)

  override def iterator: Iterator[Transformation] = record.iterator
 /* def nonEmpty = record.nonEmpty
  def size = record.size*/

/*  def addNode(id : NIdT, name : String, kind : NodeKind, styp: TypeHolder, mutable : Boolean) : RecT =
    Transformation(Add, TTNode(id, name, kind, styp, mutable)) +: this

  def removeNode(id : NIdT, name : String, kind : NodeKind, styp: TypeHolder, mutable : Boolean) : RecT =
    Transformation(Remove, TTNode(id, name, kind, styp, mutable)) +: this*/

  def addConcreteNode(n : ConcreteNode) : RecT =
    Transformation(Add, TTCNode(n)) +: this

  def addVirtualNode(n : VirtualNode) : RecT =
    Transformation(Add, TTVNode(n)) +: this


  def removeConcreteNode(n : ConcreteNode) : RecT =
    Transformation(Remove, TTCNode(n)) +: this

  def removeVirtualNode(n : VirtualNode) : RecT =
    Transformation(Remove, TTVNode(n)) +: this

  def addEdge(edge : EdgeT) : RecT =
    Transformation(Add, TTEdge(edge)) +: this

  def removeEdge(edge : EdgeT) : RecT=
    Transformation(Remove, TTEdge(edge)) +: this

  def changeEdgeTarget(edge : EdgeT, newTarget : NIdT, withMerge : Boolean) : RecT = {
    val red = if(withMerge) new RedirectionWithMerge(edge, Target(newTarget))
              else TTRedirection(edge, Target(newTarget))
    Transformation(Add, red) +: this
  }

  def changeEdgeSource(edge : EdgeT, newTarget : NIdT, withMerge : Boolean) : RecT = {
    val red = if(withMerge) new RedirectionWithMerge(edge, Source(newTarget))
    else TTRedirection(edge, Source(newTarget))
    Transformation(Add, red) +: this
  }
  def addTypeChange( typed : NIdT,
                     typ: Option[Type],
                     oldUsee: NIdT,
                     newUsee : NIdT) : RecT =
    Transformation(Add, TTTypeRedirection(typed, typ, oldUsee, newUsee)) +: this

  def addAbstraction(impl : NIdT, abs : NIdT, absPolicy : AbstractionPolicy) : RecT =
    Transformation(Add, TTAbstraction(impl, abs, absPolicy)) +: this

  def removeAbstraction(impl : NIdT, abs : NIdT, absPolicy : AbstractionPolicy) : RecT =
    Transformation(Remove, TTAbstraction(impl, abs, absPolicy)) +: this

}

sealed abstract class Operation {
  def reverse : Operation
  def productPrefix : String
}
case object Add extends Operation {
  def reverse = Remove
}
case object Remove extends Operation{
  def reverse = Add
}

case class Transformation
(operation : Operation,
 target : TransformationTarget){
  type GraphT = DependencyGraph

  def redo(g: GraphT) = target.execute(g, operation)
  def undo(g: GraphT) = target.execute(g, operation.reverse)

}



