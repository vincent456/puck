package puck.graph

/**
 * Created by lorilan on 17/06/14.
 */
class CareTakerSpec extends UnitSpec {
  val ag: AccessGraph = new AccessGraph(AGNode)

  var na: AGNode = _
  var nb: AGNode = _
  var nc: AGNode = _

  val careTaker = ag.transformations.startRegister()

  "An AG careTaker" should "be able to undo an addNode operation" in {
    na = ag.addNode("a")
    ag.nodes.toStream should contain (na)
    careTaker.undo()
    ag.nodes.toStream should not contain (na)
  }



  it should "be able to undo a sequence of addNode operation" in {
    careTaker.sequence {
      ag.addNode(na)
      nb = ag.addNode("b")
      nc = ag.addNode("c")
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

    na content_+= nb

    na.contains(nb) should be (true)

    careTaker.undo()

    na.contains(nb) should be (false)

  }

}