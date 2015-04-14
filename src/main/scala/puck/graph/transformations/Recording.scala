package puck.graph.transformations

import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}

import puck.graph._
import puck.graph.constraints.AbstractionPolicy

object Recording {
  def apply() = new Recording(Seq())

  def write(oos : ObjectOutputStream,
            r: Recording): Unit = {
    val i : Int = r.size
    oos.writeInt(i)
    r foreach oos.writeObject
  }

  def read(ois : ObjectInputStream) : Recording = {
    val recSize = ois.readInt()
    var rec = Recording()
    for(i <- Range(0, recSize)){
      rec =  ois.readObject().asInstanceOf[Transformation] +: rec
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

  def load(fileName : String, map :  Map[String, NodeId]) : Recording = {
    val (m, numIds, r) = read(fileName)
    mapNodes(r, m, map, numIds)
  }

  private def swap[A,B]( m : Map[A, B]) : Map[B, A] =
    m.toList.map {case (a,b) => (b,a)}.toMap

  def createMapping(currentIds : Map[String, NodeId],
                    newIds : Map[String, NodeId],
                    totalIds : Int ) : NodeId => NodeId = {
    assert(currentIds.size == newIds.size, "Map should be of same size")
    assert(totalIds >= currentIds.size)

    if(currentIds == newIds) identity
    else {
      val m1 = swap(currentIds).mapValues(newIds.apply)

      Range(currentIds.size, totalIds).foldLeft(m1) { case (m, id) =>
        m + (id -> id)
      }.apply
    }
  }

  def mapNodes
  (rec : Recording,
   currentIds : Map[String, NodeId],
   newIds : Map[String, NodeId], totalIds : Int) : Recording = {
    val mappin : NodeId => NodeId =
      createMapping(currentIds, newIds, totalIds)


    def mappingOnType: Type => Type = {
      case nt : NamedType => nt.copy(mappin(nt.id))
      case nt : Tuple => nt.copy(nt.types.map(mappingOnType))
      case nt : Arrow =>
        val i = nt.input
        val o = nt.output
        nt.copy(input = mappingOnType(i), output = mappingOnType(o))
    }


    def mappingOnOperation : Operation => Operation = {
      case AddCNode(n) =>
        val n2 = n.copy(id = mappin(n.id))
        AddCNode(n2.copy(styp = n.styp.map(mappingOnType)))
      case AddVNode(n) =>
        val newId = mappin(n.id)
        AddVNode(n.copy(id = newId))

      case AddEdge(e) =>
        AddEdge(e.copy(source = mappin(e.source), target = mappin(e.target)))

      case tgt : RedirectionOp =>
        val newEdge =
          mappingOnOperation(AddEdge(tgt.edge)).asInstanceOf[AddEdge].edge
        val extyId = tgt.extremity.node
        val newExty = tgt.extremity.create(mappin(extyId))
        tgt.copy(edge = newEdge, extremity = newExty)
      case AddAbstraction(impl, abs, p) =>
        AddAbstraction(mappin(impl), mappin(abs), p)

      case TypeRedirection(typed, styp, oldUsed, newUsed) =>
        TypeRedirection(mappin(typed), styp map mappingOnType,
          mappin(oldUsed), mappin(newUsed))

      case cnn @ ChangeNodeName(nid, _, _) =>
        cnn.copy(nid = mappin(nid))
    }

    rec.mapTransformation {
        case Transformation(direction, op) =>
          Transformation(direction, mappingOnOperation(op))
      }
  }

}

class Recording
(private [this] val record : Seq[Transformation]) extends Iterable[Transformation] {

  override def equals(obj : Any) : Boolean = obj match {
    case r : Recording => r() == record
    case _ => false
  }

  type NIdT = NodeId
  type EdgeT = DGEdge
  type RecT = Recording
  def apply() = record

  private def mapTransformation(f : Transformation => Transformation) : Recording =
    new Recording (record map f)


  def +:(r : Transformation) : Recording =
    new Recording(r +: record)

  override def iterator: Iterator[Transformation] = record.reverseIterator
 /* def nonEmpty = record.nonEmpty
  def size = record.size*/

/*  def addNode(id : NIdT, name : String, kind : NodeKind, styp: TypeHolder, mutable : Boolean) : RecT =
    Transformation(Add, TTNode(id, name, kind, styp, mutable)) +: this

  def removeNode(id : NIdT, name : String, kind : NodeKind, styp: TypeHolder, mutable : Boolean) : RecT =
    Transformation(Remove, TTNode(id, name, kind, styp, mutable)) +: this*/

  def addConcreteNode(n : ConcreteNode) : RecT =
    Transformation(Regular, AddCNode(n)) +: this

  def addVirtualNode(n : VirtualNode) : RecT =
    Transformation(Regular, AddVNode(n)) +: this


  def changeNodeName(nid : NodeId, oldName : String, newName : String) : RecT =
    Transformation(Regular, ChangeNodeName(nid, oldName, newName)) +: this

  def removeConcreteNode(n : ConcreteNode) : RecT =
    Transformation(Reverse, AddCNode(n)) +: this

  def removeVirtualNode(n : VirtualNode) : RecT =
    Transformation(Reverse, AddVNode(n)) +: this

  def addEdge(edge : EdgeT) : RecT =
    Transformation(Regular, AddEdge(edge)) +: this

  def removeEdge(edge : EdgeT) : RecT=
    Transformation(Reverse, AddEdge(edge)) +: this

  def changeEdgeTarget(edge : EdgeT, newTarget : NIdT, withMerge : Boolean) : RecT = {
    val red = if(withMerge) new RedirectionWithMerge(edge, Target(newTarget))
              else RedirectionOp(edge, Target(newTarget))
    Transformation(Regular, red) +: this
  }

  def changeEdgeSource(edge : EdgeT, newTarget : NIdT, withMerge : Boolean) : RecT = {
    val red = if(withMerge) new RedirectionWithMerge(edge, Source(newTarget))
    else RedirectionOp(edge, Source(newTarget))
    Transformation(Regular, red) +: this
  }
  def addTypeChange( typed : NIdT,
                     typ: Option[Type],
                     oldUsee: NIdT,
                     newUsee : NIdT) : RecT =
    Transformation(Regular, TypeRedirection(typed, typ, oldUsee, newUsee)) +: this

  def addAbstraction(impl : NIdT, abs : NIdT, absPolicy : AbstractionPolicy) : RecT =
    Transformation(Regular, AddAbstraction(impl, abs, absPolicy)) +: this

  def removeAbstraction(impl : NIdT, abs : NIdT, absPolicy : AbstractionPolicy) : RecT =
    Transformation(Reverse, AddAbstraction(impl, abs, absPolicy)) +: this

}

sealed abstract class Direction {
  def reverse : Direction
  def productPrefix : String
}
case object Regular extends Direction {
  def reverse = Reverse
}
case object Reverse extends Direction{
  def reverse = Regular
}

case class Transformation
(operation : Direction,
 target : Operation){
  type GraphT = DependencyGraph

  def redo(g: GraphT) = target.execute(g, operation)
  def undo(g: GraphT) = target.execute(g, operation.reverse)

}



