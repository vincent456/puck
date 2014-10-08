package puck.graph

import java.io.FileOutputStream

import puck.graph.backTrack._
import puck.graph.backTrack.comparison.{RecordingComparator, NodeMappingInitialState}
import puck.graph.constraints.{DelegationAbstraction, SupertypeAbstraction}
import puck.javaAG.nodeKind.JavaNodeKind
import puck.javaAG.{JavaAccessGraph, JavaFilesHandler}
import puck.util.{DefaultFileLogger, DefaultSystemLogger}

/**
 * Created by lorilan on 17/06/14.
 */
class CareTakerSpec extends UnitSpec {
  val ag: AccessGraph[VanillaKind] = new AccessGraph(AGNode)

  var na : AGNode[VanillaKind] = _
  var nb : AGNode[VanillaKind] = _
  var nc : AGNode[VanillaKind] = _

  val careTaker = ag.transformations.startRegister()
  val breakPoint = ag.transformations.startSequence()

  "A transformation target" should "be reflexive with regards to equality" in {
    val n0 = ag.addNode("n0", VanillaNodeKind())
    val n1 = ag.addNode("n1", VanillaNodeKind())
    val n2 = ag.addNode("n2", VanillaNodeKind())
    val n3 = ag.addNode("n3", VanillaNodeKind())

    TTEdge(AGEdge.uses(n0, n1)) should equal (TTEdge(AGEdge.uses(n0, n1)))
    TTRedirection(AGEdge.uses(n0, n1), Source(n2)) should equal (TTRedirection(AGEdge.uses(n0, n1), Source(n2)))
    TTRedirection(AGEdge.uses(n0, n1), Target(n3)) should equal (TTRedirection(AGEdge.uses(n0, n1), Target(n3)))
    TTDependency(AGEdge.uses(n0, n1), AGEdge.uses(n2, n3)) should equal (TTDependency(AGEdge.uses(n0, n1), AGEdge.uses(n2, n3)))
    TTAbstraction(n0, n1, SupertypeAbstraction()) should equal (TTAbstraction(n0, n1, SupertypeAbstraction()))
    TTAbstraction(n0, n1, DelegationAbstraction()) should equal (TTAbstraction(n0, n1, DelegationAbstraction()))
  }

  "An AG careTaker" should "be able to undo an addNode operation" in {
    na = ag.addNode("a", VanillaNodeKind())
    ag.nodes.toStream should contain (na)
    careTaker.undo(breakPoint)
    ag.nodes.toStream should not contain (na)
  }



  it should "be able to undo a sequence of addNode operation" in {

    ag.addNode(na)
    nb = ag.addNode("b", VanillaNodeKind())
    nc = ag.addNode("c", VanillaNodeKind())


    ag.nodes.toStream should contain (na)
    ag.nodes.toStream should contain (nb)
    ag.nodes.toStream should contain (nc)

    careTaker.undo(breakPoint)

    ag.nodes.toStream should not contain (na)
    ag.nodes.toStream should not contain (nb)
    ag.nodes.toStream should not contain (nc)

  }

  it should "be able to undo the creation of a contains arc" in {

    na.content += nb

    na.contains(nb) should be (true)

    careTaker.undo(breakPoint)

    na.contains(nb) should be (false)
  }

  it should "be able to undo the creation of an edge dependency" in {
    val ag = new AccessGraph[VanillaKind](AGNode)
    ag.transformations.startRegister()
    val na = ag.addNode("a", VanillaNodeKind())
    val nb = ag.addNode("b", VanillaNodeKind())
    val nc = ag.addNode("c", VanillaNodeKind())

    ag.root.content += na
    ag.root.content += nb
    nb.content += nc

    nb.users += na
    nc.users += na

    val bp = ag.transformations.startSequence()

    ag.addUsesDependency(AGEdge.uses(na, nb), AGEdge.uses(na, nc))

    na.sideUses(nb).toStream should contain (AGEdge.uses(na, nc))
    na.primaryUses(nc).toStream should contain (AGEdge.uses(na, nb))
    (na uses nb) should be (true)
    (na uses nc) should be (true)

    ag.transformations.undo(bp)

    na.sideUses.get(nb) should be (None)
    na.primaryUses.get(nc) should be (None)

    (na uses nb) should be (true)
    (na uses nc) should be (true)

  }

  it should "be able to undo a merge" in {

    val fh = new JavaFilesHandler(new java.io.File("/home/lorilan/projects/constraintsSolver/scala/" +
      "src/test/resources/examples/merge/"))
    val jgraph : JavaAccessGraph = fh.loadGraph()

    val (_, tranfos) = NodeMappingInitialState.normalizeNodeTransfos(jgraph.transformations.recording.composition)
    jgraph.initialRecord = tranfos
    val breakPoint = jgraph.transformations.startSequence()
    val i : AGNode[JavaNodeKind] = jgraph("p.I")
    val i2 : AGNode[JavaNodeKind] = jgraph("p.I2")
    val a  : AGNode[JavaNodeKind] = jgraph("p.A")

    fh.makePng(printId = true,
      sOutput = Some(new FileOutputStream(fh.graphFile( "_0.png"))))()


    i mergeWith i2

    fh.makePng(printId = true,
      sOutput = Some(new FileOutputStream(fh.graphFile( "_1.png"))))()

    val rec = jgraph.transformations.recording

    println("recording : ")
    rec foreach {t => println("\t"+ t)}

    jgraph.transformations.undo(breakPoint)
    fh.makePng(printId = true,
      sOutput = Some(new FileOutputStream(fh.graphFile( "_2.png"))))()

    i2 mergeWith i

    fh.makePng(printId = true,
      sOutput = Some(new FileOutputStream(fh.graphFile( "_3.png"))))()

    val rec2 = jgraph.transformations.recording

    new RecordingComparator(jgraph.initialRecord, rec, rec2, new DefaultFileLogger(fh.graphFile(".log"))).search() should not be (None)

  }

}