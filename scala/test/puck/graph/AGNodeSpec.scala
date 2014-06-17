package puck.graph

/**
 * Created by lorilan on 17/06/14.
 */
class AGNodeSpec extends UnitSpec {

  val ag : AccessGraph = new AccessGraph(AGNode)
  val na = ag.addNode("a")
  val nb = ag.addNode("b")
  val nc = ag.addNode("c")

  "An access graph node" should "contains what is added in its content" in {

    na.contains(nb) should be (false)
    na.content_+=(nb)
    na.contains(nb) should be (true)
    nb.container should be (na)
  }

  it should "uses what is added in its uses" in {
    na.uses(nc) should be (false)
    na.uses_+=(nc)
    na.uses(nc) should be (true)
  }

  it should "be what is added in its super types" in {
    nb.isa(nc) should be (false)
    nb.superTypes_+=(nc)
    nb.isa(nc) should be (true)
  }

  it should "be equals to himself" in {
    nb should equal (nb)
  }

  it should "have a fullName composed by its ancestor's local name" in {
    ag.root.content_+=(na)
    nb.content_+=(nc)
    nc.fullName should be ("a.b.c")
  }

  it should "be marked as unrooted if it has no container" in {
    nb.content_-=(nc)
    nc.fullName should be (AccessGraph.unrootedStringId + ag.scopeSeparator +"c")
  }
}

