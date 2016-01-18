package puck.graph
package transformations

import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}

import puck.graph.comparison.Mapping




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
  def load(fileName : String, map :  Map[String, NodeId]) : Recording = {
    val (m, numIds, r) = read(fileName)
    try mapNodes(r, m, map, numIds)
    catch {
      case e : Error =>
        throw LoadError(e.getMessage, m)
      case e : Exception =>
        throw LoadError(e.getMessage, m)
    }
  }



  def createMapping(recordIds : Map[String, NodeId],
                    newIds : Map[String, NodeId],
                    totalIds : Int ) : NodeId => NodeId = {
    assert(recordIds.size == newIds.size, "Map should be of same size")
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
   recordIds : Map[String, NodeId],
   newIds : Map[String, NodeId], totalIds : Int) : Recording = {
    val mappin : NodeId => NodeId =
      createMapping(recordIds, newIds, totalIds)

    val mappinNodeIdP : NodeIdP => NodeIdP = {
      case (n1, n2) => (mappin(n1), mappin(n2))
    }

    val mappingOnType = Mapping.mapType(mappin)

    val mappingOnRole : Role => Role = {
      case Initializer(id) => Initializer(mappin(id))
      case Factory(id) => Factory(mappin(id))
    }

    def mappingOnOperation : Operation => Operation = {
      case CNode(n) =>
        CNode(n.copy(id = mappin(n.id)))
      case VNode(n) =>
        val newId = mappin(n.id)
        VNode(n.copy(id = newId))

      case Edge(e) =>
        Edge(e.kind(source = mappin(e.source), target = mappin(e.target)))

      case tgt : RedirectionOp =>
        val newEdge =
          mappingOnOperation(Edge(tgt.edge)).asInstanceOf[Edge].edge
        val extyId = tgt.extremity.node
        val newExty = tgt.extremity.create(mappin(extyId))
        tgt.copy(edge = newEdge, extremity = newExty)
      case AbstractionOp(impl, AccessAbstraction(abs, p)) =>
        AbstractionOp(mappin(impl), AccessAbstraction(mappin(abs), p))

      case AbstractionOp(impl, ReadWriteAbstraction(sRabs, sWabs)) =>
        AbstractionOp(mappin(impl), ReadWriteAbstraction(sRabs map mappin, sWabs map mappin))


      case TypeChange(typed, oldType, newType) =>
        TypeChange(mappin(typed), oldType map mappingOnType, newType map mappingOnType)

      case cnn @ ChangeNodeName(nid, _, _) =>
        cnn.copy(nid = mappin(nid))
      case ChangeTypeBinding((e1,e2), binding) =>
        ChangeTypeBinding((mappinNodeIdP(e1),mappinNodeIdP(e2)),
          binding.create(mappinNodeIdP(binding.edge)))
      case TypeDependency(tUse, tmUse) =>
        TypeDependency(mappinNodeIdP(tUse),mappinNodeIdP(tmUse))
      case RoleChange(id, sor, snr) =>
        RoleChange(mappin(id), sor map mappingOnRole, snr map mappingOnRole)

    }



    rec.mapTransformation {
        case Transformation(direction, op) =>
          Transformation(direction, mappingOnOperation(op))
      }
  }


  implicit class RecordOps(val r : Recording ) extends AnyVal {
    def subRecordFromLastMilestone : Recording = {
      def f(r0 : Recording, acc : Recording) : Recording = r0 match {
        case Nil => acc
        case MileStone +: tl => acc
        case hd +: tl => f(tl, hd +: acc)
      }

      f(r, Seq()).reverse
    }

    def involveNodes : Seq[NodeId] = r.foldLeft(Seq[NodeId]()){
      case (acc, Transformation(_, op)) =>
        Operation.involvedNodes(op) ++: acc
      case (acc, _) => acc

    }
  }
}







