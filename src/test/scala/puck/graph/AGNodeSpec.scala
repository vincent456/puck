/*
package puck.graph

import puck.graph.mutable.{AGNode, AccessGraph, VanillaNodeKind, VanillaKind}

/**
 * Created by lorilan on 17/06/14.
 */
class AGNodeSpec extends UnitSpec {

  val ag : DependencyGraph[VanillaKind] = new DependencyGraph(DGNode)
  val na = ag.addNode("a", VanillaNodeKind())
  val nb = ag.addNode("b", VanillaNodeKind())
  val nc = ag.addNode("c", VanillaNodeKind())

  "An access graph node" should "contains what is added in its content" in {

    na.contains(nb) should be (false)
    na.content += nb
    na.contains(nb) should be (true)
    nb.container should be (na)
  }

  it should "uses what is added in its uses" in {
    na.uses(nc) should be (false)
    na.uses += nc
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
    ag.rootId.content += na
    nb.content += nc
    nc.fullName should be ("a.b.c")
  }

  it should "be marked as unrooted if it has no container" in {
    nb.content -= nc
    nc.fullName should be (DependencyGraph.unrootedStringId + ag.scopeSeparator +"c")
  }

  it should "have a distance from another node equal to the length of the path composed of contains edge only between the two nodes" in {
    val ag : DependencyGraph[VanillaKind] = new DependencyGraph(DGNode)
    val na = ag.addNode("a", VanillaNodeKind())
    val nb = ag.addNode("b", VanillaNodeKind())
    val nc = ag.addNode("c", VanillaNodeKind())
    val nd = ag.addNode("d", VanillaNodeKind())
    val ne = ag.addNode("e", VanillaNodeKind())
    val nf = ag.addNode("f", VanillaNodeKind())
    val ng = ag.addNode("g", VanillaNodeKind())

    ag.rootId.content += na
    ag.rootId.content += nb
    na.content += nc
    na.content += nd
    nc.content += ne
    nd.content += nf
    nb.content += ng


    /*
                       root
                        /\
                       a  b
                      /\  \
                     c d   g
                    /  \
                   e   f
                 distance(c,d) = 2
                 distance(c,a) = 1
                 distance(c,b) = 3
                 distance(e,f) = 4
                 distance(e,g) = 5
     */
    nc.distance(nd) should be (2)
    nd.distance(nc) should be (2)

    nc.distance(na) should be (1)
    na.distance(nc) should be (1)

    nc.distance(nb) should be (3)
    nb.distance(nc) should be (3)

    ne.distance(nf) should be (4)
    nf.distance(ne) should be (4)

    ne.distance(ng) should be (5)
    ng.distance(ne) should be (5)

  }
}

*/
