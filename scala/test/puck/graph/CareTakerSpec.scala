package puck.graph

/**
 * Created by lorilan on 17/06/14.
 */
class CareTakerSpec extends UnitSpec {
  val ag: AccessGraph[VanillaKind] = new AccessGraph(AGNode)

  var na: AGNode[VanillaKind] = _
  var nb: AGNode[VanillaKind] = _
  var nc: AGNode[VanillaKind] = _

  val careTaker = ag.transformations.startRegister()

  "An AG careTaker" should "be able to undo an addNode operation" in {
    na = ag.addNode("a", VanillaNodeKind())
    ag.nodes.toStream should contain (na)
    careTaker.undo()
    ag.nodes.toStream should not contain (na)
  }



  it should "be able to undo a sequence of addNode operation" in {
    careTaker.sequence {
      ag.addNode(na)
      nb = ag.addNode("b", VanillaNodeKind())
      nc = ag.addNode("c", VanillaNodeKind())
    }

    ag.nodes.toStream should contain (na)
    ag.nodes.toStream should contain (nb)
    ag.nodes.toStream should contain (nc)

    careTaker.undo()

    ag.nodes.toStream should not contain (na)
    ag.nodes.toStream should not contain (nb)
    ag.nodes.toStream should not contain (nc)

  }

  it should "be able to undo the creation of a contains arc" in {

    na.content += nb

    na.contains(nb) should be (true)

    careTaker.undo()

    na.contains(nb) should be (false)
  }

  it should "be able to undo the creation of an edge dependency" in {
    val ag = new AccessGraph[VanillaKind](AGNode)
    val na = ag.addNode("a", VanillaNodeKind())
    val nb = ag.addNode("b", VanillaNodeKind())
    val nc = ag.addNode("c", VanillaNodeKind())

    ag.root.content += na
    ag.root.content += nb
    nb.content += nc

    nb.users_+=(na)
    nc.users_+=(na)

    ag.transformations.register {
      ag.addUsesDependency(AGEdge.uses(na, nb), AGEdge.uses(na, nc))

      na.sideUses(nb).toStream should contain (AGEdge.uses(na, nc))
      na.primaryUses(nc).toStream should contain (AGEdge.uses(na, nb))
      (na uses nb) should be (true)
      (na uses nc) should be (true)


      ag.transformations.undo()

      na.sideUses.get(nb) should be (None)
      na.primaryUses.get(nc) should be (None)

      (na uses nb) should be (true)
      (na uses nc) should be (true)

    }
  }

}