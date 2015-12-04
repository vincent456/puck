package puck.javaGraph

import puck._
import puck.graph.transformations.rules.Move
import puck.javaGraph.transformations.{JavaIntro, JavaAbstract}

class MoveUnitSpec extends UnitSpec {
  val examplesPath = Settings.testExamplesPath + "/move"
  val moveMethodNotUsedByThis = examplesPath + "/method/NOTusedByThis"
  val moveMethodUsedByThis = examplesPath + "/method/usedByThis"

  "Move" should "know that a typeMember is NOT used by self type" in {
    val _ = new ScenarioFactory(s"$moveMethodNotUsedByThis/MovedMethodHasNoParameter.java"){

      val classA = graph.getConcreteNode(fullName2id("p.A"))
      val methToMove = graph.getConcreteNode(fullName2id("p.A.methodToMove()"))

      val uses = graph.usesFromUsedList(methToMove.id)
      assert(!Move.usedBySiblingsViaSelf(uses, graph, classA))

    }
  }

  it should "know that a typeMember is used by self type" in {

    val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java") {

      val classA = graph.getConcreteNode(fullName2id("p.A"))
      val methToMove = graph.getConcreteNode(fullName2id("p.A.methodToMove()"))

      val uses = graph.usesFromUsedList(methToMove.id)
      assert(Move.usedBySiblingsViaSelf(uses, graph, classA))
    }
  }
  it should "know that a typeMember is used by self type when used several times" in {
    val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodUsedByThisSeveralTimes.java") {

      val classA = graph.getConcreteNode(fullName2id("p.A"))
      val methToMove = graph.getConcreteNode(fullName2id("p.A.methodToMove()"))

      val uses = graph.usesFromUsedList(methToMove.id)
      assert(Move.usedBySiblingsViaSelf(uses, graph, classA))
    }
  }

}
