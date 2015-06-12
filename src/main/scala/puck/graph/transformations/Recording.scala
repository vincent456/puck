package puck.graph
package transformations

import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}




object Recording {
  def apply() = Seq[Recordable]()

  def write(oos : ObjectOutputStream,
            r: Recording): Unit = {
    val i : Int = r.size
    oos.writeInt(i)
    r.reverse foreach oos.writeObject
  }

  def read(ois : ObjectInputStream) : Recording = {
    val recSize = ois.readInt()
    var rec = Recording()
    for(i <- Range(0, recSize)){
      rec =  ois.readObject().asInstanceOf[Recordable] +: rec
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
      case CNode(n) =>
        val n2 = n.copy(id = mappin(n.id))
        CNode(n2.copy(styp = n.styp.map(mappingOnType)))
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


      case TypeRedirection(typed, oldUsed, newUsed) =>
        TypeRedirection(mappin(typed), mappin(oldUsed), mappin(newUsed))

      case cnn @ ChangeNodeName(nid, _, _) =>
        cnn.copy(nid = mappin(nid))
      case TypeDependency((tUser, tUsed), (tmUser, tmUsed)) =>
        TypeDependency((mappin(tUser), mappin(tUsed)),
          (mappin(tmUser), mappin(tmUsed)))

    }



    rec.mapTransformation {
        case Transformation(direction, op) =>
          Transformation(direction, mappingOnOperation(op))
      }
  }

}







