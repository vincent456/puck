package puck.javaGraph

import puck._
import puck.graph.transformations.rules.Move
import puck.javaGraph.transformations.JavaIntro

class MoveUnitSpec extends UnitSpec {
  val examplesPath = Settings.testExamplesPath + "/move"
  val moveMethodNotUsedByThis = examplesPath + "/method/notUsedByThis"
  val moveMethodUsedByThis = examplesPath + "/method/usedByThis"

  val Move = new Move(JavaIntro)

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
