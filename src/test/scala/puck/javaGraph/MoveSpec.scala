package puck
package javaGraph

import java.io.{PrintStream, FileWriter, FileOutputStream}

import puck.graph._
import puck.graph.transformations.rules.{CreateTypeMember, CreateParameter, Move}
import puck.javaGraph.nodeKind.Field
import puck.javaGraph.transformations.{JavaIntro, JavaAbstract}

import scalaz.{\/-, -\/}

class MoveSpec extends AcceptanceSpec {

  val Move = new Move(JavaIntro)

  val examplesPath = Settings.testExamplesPath + "/move"

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

        assertSuccess(Move.typeDecl(graph, classA, package2).value) {
          g2 =>
            assert(g2.container(classA).value == package2)
            assert(graph.uses(methB, classA))
            assert(graph.uses(methB, methA))
        }
      }
    }
  }

  val moveMethodNotUsedByThis = examplesPath + "/method/notUsedByThis"
  val moveMethodUsedByThis = examplesPath + "/method/usedByThis"

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



  feature("Move method"){

    scenario("moved method not used by this"){
      val _ = new ExampleSample(s"$moveMethodNotUsedByThis/MovedMethodHasNoParameter.java"){

        val classA = fullName2id("p.A")
        val methToMove = fullName2id("p.A.methodToMove__void")
        val methUser = fullName2id("p.C.user__void")

        val classB = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)
        assert(graph.uses(methUser, methToMove))

        assertSuccess(Move.typeMember(graph,
          List(methToMove), classB)().value){
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

        assertSuccess(Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateParameter))().value){
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

        assertSuccess(Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateTypeMember(Field)))().value){
          g2 =>
            //quickFrame(g2)
            val ma2Delegate =
              g2.content(classA).find{
                id => g2.getConcreteNode(id).name == "b_delegate"
              }.value

            assert(g2.container(methToMove).value == newHostClass)

            assert(g2.uses(methUser, methToMove))
            assert(g2.uses(ma2Delegate, newHostClass))
            assert(g2.uses(methUser, ma2Delegate))
        }
      }
    }


    scenario("move method used by this several times - keep reference with Parameter"){


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

        assertSuccess(Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateParameter))().value){
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

        val g2 =
          try {
            Move.typeMember(graph, List(methToMove), newHostClass,
              Some(CreateTypeMember(Field)))().value.value
          }
          catch{
            case t: Throwable =>
              t.printStackTrace( new PrintStream(new FileOutputStream("/tmp/errorTrace")) )
              graph
          }

        val methToMoveDelegateList =
          g2.content(classA).filter {
            id =>
              g2.getConcreteNode(id).name startsWith "b_delegate"
          }

        assert(methToMoveDelegateList.size == 1)
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

        assertSuccess(Move.typeMember(graph, List(methMa), classB)().value){
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

  feature("Move methods"){

    scenario("one of the moved is used by another") {
      val _ = new ExampleSample(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java") {

        val classA = fullName2id("p.A")
        val methUser = fullName2id("p.A.mUser__void")
        val methToMove = fullName2id("p.A.methodToMove__void")

        val newHostClass = fullName2id("p.B")

        assertSuccess(Move.typeMember(graph, List(methToMove, methUser), newHostClass, None)().value) {
          g2 =>
            g2.content(classA).size shouldBe (graph.content(classA).size - 2)
            assert(g2.container(methToMove).value == newHostClass)
            assert(g2.container(methUser).value == newHostClass)
            assert(g2.uses(methUser, methToMove))
            assert(!g2.uses(methUser, newHostClass))
        }

      }
    }

    scenario("two moved method both used by an unmoved one") {
      val _ = new ExampleSample(s"$moveMethodUsedByThis/TwoMovedMethodUsedBySameUnmovedSibling.java") {

        val classA = fullName2id("p.A")
        val methUser = fullName2id("p.A.mUser__void")
        val methToMove1 = fullName2id("p.A.methodToMove1__void")
        val methToMove2 = fullName2id("p.A.methodToMove2__void")

        val newHostClass = fullName2id("p.B")

        val numArgs = getNumArgs(graph.getConcreteNode(methUser))

        assertSuccess(Move.typeMember(graph, List(methToMove1, methToMove2), newHostClass, Some(CreateParameter))().value) {
          g2 =>

            g2.content(classA).size shouldBe (graph.content(classA).size - 2)
            getNumArgs(g2.getConcreteNode(methUser)) shouldBe (numArgs + 1)
            assert(g2.container(methToMove1).value == newHostClass)
            assert(g2.container(methToMove2).value == newHostClass)

            assert(g2.uses(methUser, methToMove1))
            assert(g2.uses(methUser, methToMove2))
        }

      }
    }
  }

}
