package puck.javaGraph

import puck._
import puck.graph.transformations.rules.Move

/**
 * Created by lorilan on 4/3/15.
 */
class MoveUnitSpec extends UnitSpec {
  val examplesPath = puck.testExamplesPath + "/move"
  val moveMethodNotUsedByThis = examplesPath + "/method/notUsedByThis"
  val moveMethodUsedByThis = examplesPath + "/method/usedByThis"


  "Move" should "know that a typeMember is NOT used by self type" in {
    val _ = new ExampleSample(s"$moveMethodNotUsedByThis/MovedMethodHasNoParameter.java"){

      val classA = graph.getConcreteNode(fullName2id("p.A"))
      val methToMove = graph.getConcreteNode(fullName2id("p.A.methodToMove__void"))

      assert(!Move.isUsedBySiblingsViaSelf(graph, methToMove, classA))

    }
  }

  it should "know that a typeMember is used by self type" in {

    val _ = new ExampleSample(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java") {

      val classA = graph.getConcreteNode(fullName2id("p.A"))
      val methToMove = graph.getConcreteNode(fullName2id("p.A.methodToMove__void"))

      assert(Move.isUsedBySiblingsViaSelf(graph, methToMove, classA))
    }
  }
  it should "know that a typeMember is used by self type when used several times" in {
    val _ = new ExampleSample(s"$moveMethodUsedByThis/UsedBySelfSeveralTimes.java") {

      val classA = graph.getConcreteNode(fullName2id("p.A"))
      val methToMove = graph.getConcreteNode(fullName2id("p.A.methodToMove__void"))

      assert(Move.isUsedBySiblingsViaSelf(graph, methToMove, classA))
    }
  }

}
