package puck
package javaGraph

import puck.graph._
import puck.graph.transformations.rules.Move
import Move.CreateTypeMember
import Move.CreateParameter
import puck.javaGraph.nodeKind.Field

import scalaz.{\/-, -\/}

/**
 * Created by lorilan on 4/2/15.
 */
class MoveSpec extends AcceptanceSpec {

  def assertSuccess[G](t : Try[G])(f : G => Unit) : Unit = {
    t match {
      case -\/(_) => assert(false)
      case \/-(g) => f(g)
    }
  }

  val examplesPath = puck.testExamplesPath + "/move"

  feature("Move class") {

    scenario("Move top level class") {
      val p = "topLevelClass"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java",
        s"$examplesPath/$p/Empty.java") {
        val package1 = fullName2id(s"$p.p1")
        val package2 = fullName2id(s"$p.p2")
        val classA = fullName2id(s"$p.p1.A")
        val methA = fullName2id(s"$p.p1.A.ma__void")

        val classB = fullName2id(s"$p.p1.B")
        val methB = fullName2id(s"$p.p1.B.mb__void")

        assert(graph.container(classA).value == package1)
        assert(graph.uses(methB, classA))
        assert(graph.uses(methB, methA))

        assertSuccess(Move.moveTypeDecl(graph, classA, package2)) {
          g2 =>
            assert(g2.container(classA).value == package2)
            assert(graph.uses(methB, classA))
            assert(graph.uses(methB, methA))
        }
      }
    }
  }

  feature("Move method"){

    val moveMethodNotUsedByThis = examplesPath + "/method/notUsedByThis"
    val moveMethodUsedByThis = examplesPath + "/method/usedByThis"

    scenario("moved method not used by this"){
      val _ = new ExampleSample(s"$moveMethodNotUsedByThis/MovedMethodHasNoParameter.java"){

        val classA = fullName2id("p.A")
        val methToMove = fullName2id("p.A.methodToMove__void")
        val methUser = fullName2id("p.C.user__void")

        val classB = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)
        assert(graph.uses(methUser, methToMove))

        assertSuccess(Move.moveTypeMember(graph,
          methToMove, classB)){
          g2 =>

            assert(g2.container(methToMove).value == classB)
            assert(g2.uses(methUser, methToMove))
        }
      }
    }

    scenario("move method used by this - keep reference with parameter"){
      val _ = new ExampleSample(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java"){

        val classA = fullName2id("p.A")
        val methUser = fullName2id("p.A.mUser__void")
        val methToMove = fullName2id("p.A.methodToMove__void")

        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)
        assert(graph.uses(methUser, methToMove))
        assert(! graph.uses(methUser, newHostClass))

        assertSuccess(Move.moveTypeMember(graph, methToMove, newHostClass, CreateParameter)){
          g2 =>
            g2.content(classA).size shouldBe (graph.content(classA).size - 1)
            assert(g2.container(methToMove).value == newHostClass)
            assert(g2.uses(methUser, methToMove))
            assert(g2.uses(methUser, newHostClass))
        }
      }
    }

    scenario("move method used by this - keep reference with Field"){
      val _ = new ExampleSample(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java"){

        val classA = fullName2id("p.A")
        val methUser = fullName2id("p.A.mUser__void")
        val methToMove = fullName2id("p.A.methodToMove__void")

        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)
        assert(graph.uses(methUser, methToMove))
        assert(! graph.uses(methUser, newHostClass))

        assertSuccess(Move.moveTypeMember(graph, methToMove, newHostClass, CreateTypeMember(Field))){
          g2 =>
            //quickFrame(g2)
            val ma2Delegate =
              g2.content(classA).find{
                id => g2.getConcreteNode(id).name == "methodToMove_delegate"
              }.value

            assert(g2.container(methToMove).value == newHostClass)

            assert(g2.uses(methUser, methToMove))
            assert(g2.uses(ma2Delegate, newHostClass))
            assert(g2.uses(methUser, ma2Delegate))
        }
      }
    }

    scenario("move method used by this several times - keep reference with Parameter"){

      def assertIsArrowAndUncurry(t : Type) : Arrow = {
        t match {
          case a : Arrow => a.uncurry
          case _ => assert(false)
            Arrow(NamedType(0), NamedType(0))
        }
      }
      def assertIsTupleAndGetSize : Type => Int = {
        case Tuple(ts) => ts.size
        case _ => assert(false)
          0
      }

      def getNumArgs(n : ConcreteNode) =
        assertIsTupleAndGetSize(assertIsArrowAndUncurry(n.styp.value).input)

      val _ = new ExampleSample(s"$moveMethodUsedByThis/UsedBySelfSeveralTimes.java"){

        val classA = fullName2id("p.A")
        val methUser1 = fullName2id("p.A.mUser1__void")
        val methUser2 = fullName2id("p.A.mUser2__void")
        val methToMove = fullName2id("p.A.methodToMove__void")
        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)
        assert(graph.uses(methUser1, methToMove))
        assert(! graph.uses(methUser1, newHostClass))

        assert(graph.uses(methUser2, methToMove))
        assert(! graph.uses(methUser2, newHostClass))

        val numArgs1 = getNumArgs(graph.getConcreteNode(methUser1))
        val numArgs2 = getNumArgs(graph.getConcreteNode(methUser2))

        assertSuccess(Move.moveTypeMember(graph, methToMove, newHostClass, CreateParameter)){
          g2 =>
            //quickFrame(g2)

            assert(numArgs1 + 1 == getNumArgs(g2.getConcreteNode(methUser1)))
            assert(numArgs2 + 1 == getNumArgs(g2.getConcreteNode(methUser2)))

            assert(g2.container(methToMove).value == newHostClass)

            assert(g2.uses(methUser1, methToMove))
            assert(g2.uses(methUser1, newHostClass))

            assert(g2.uses(methUser2, methToMove))
            assert(g2.uses(methUser2, newHostClass))
        }
      }
    }

    scenario("move method used by this several times - keep reference with Field"){
      val _ = new ExampleSample(s"$moveMethodUsedByThis/UsedBySelfSeveralTimes.java"){

        val classA = fullName2id("p.A")
        val methUser1 = fullName2id("p.A.mUser1__void")
        val methUser2 = fullName2id("p.A.mUser2__void")
        val methToMove = fullName2id("p.A.methodToMove__void")

        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)
        assert(graph.uses(methUser1, methToMove))
        assert(graph.uses(methUser2, methToMove))

        assertSuccess(Move.moveTypeMember(graph, methToMove, newHostClass, CreateTypeMember(Field))){
          g2 =>
            val methToMoveDelegateList =
              g2.content(classA).filter{
                id =>
                  g2.getConcreteNode(id).name startsWith "methodToMove_delegate"
              }

            assert( methToMoveDelegateList.size == 1)
            val methToMoveDelegate = methToMoveDelegateList.head

            assert(g2.container(methToMove).value == newHostClass)

            assert(g2.uses(methUser2, methToMove))
            assert(g2.uses(methToMoveDelegate, newHostClass))
            assert(g2.uses(methUser2, methToMoveDelegate))

            assert(g2.uses(methUser1, methToMove))
            assert(g2.uses(methToMoveDelegate, newHostClass))
            assert(g2.uses(methUser1, methToMoveDelegate))
        }
      }
    }

    ignore("Move method not used by this to class of a parameter"){
      val _ = new ExampleSample(s"$moveMethodNotUsedByThis/MovedMethodHasOneParameterNotTypedAsSelf.java"){

        val rootPackage = fullName2id("p")

        val classA = fullName2id("p.A")
        val methMa = fullName2id("p.A.ma__B")
        val methUser = fullName2id("p.C.mc__void")

        val classB = fullName2id("p.B")

        val methMaNode = graph.getConcreteNode(methMa)
        methMaNode.styp.value match {
          case MethodType(in, _) =>
            assert(1 == in.length && in.ids.contains(classB))
        }

        assert(graph.container(methMa).value == classA)
        assert(graph.uses(methUser, methMa))

        assertSuccess(Move.moveTypeMember(graph, methMa, classB)){
          g2 =>

            assert(g2.container(methMa).value == classB)
            assert(g2.uses(methUser, methMa))

            val methMaNode = g2.getConcreteNode(methMa)
            methMaNode.styp.value match {
              case MethodType(in, _) => assert(0 == in.length)
            }
        }
      }
    }

  }
}
