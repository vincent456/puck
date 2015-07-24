package puck
package javaGraph

import puck.graph._
import puck.graph.transformations.rules.{CreateTypeMember, CreateParameter, Move}
import puck.javaGraph.nodeKind.Field

class MoveSpec extends AcceptanceSpec {

  val examplesPath = Settings.testExamplesPath + "/move"

  def getDefinition(g : DependencyGraph, nid : NodeId) : NodeId =
    g.getConcreteNode(nid).definition(g).value


  feature("Move class") {

    scenario("Move top level class") {
      val p = "topLevelClass"
      val _ = new ExampleSample(s"$examplesPath/$p/A.java",
        s"$examplesPath/$p/Empty.java") {
        val package1 = fullName2id(s"$p.p1")
        val package2 = fullName2id(s"$p.p2")
        val classA = fullName2id(s"$p.p1.A")
        val methADecl = fullName2id(s"$p.p1.A.ma__void")
        val methADef = getDefinition(graph, methADecl)

        val classB = fullName2id(s"$p.p1.B")
        val methBDecl = fullName2id(s"$p.p1.B.mb__void")
        val methBDef = getDefinition(graph, methBDecl)


        assert(graph.container(classA).value == package1)
        assert(graph.uses(methBDef, classA))
        assert(graph.uses(methBDef, methADecl))

        val g2 = Move.typeDecl(graph, classA, package2).right
        assert(g2.container(classA).value == package2)
        assert(g2.uses(methBDef, classA))
        assert(g2.uses(methBDef, methADecl))

      }

    }
  }

  val moveMethod_movedMethodUsesThis = examplesPath + "/method/movedMethodUsesThis"
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

  feature("Move method"){

    scenario("moved method not used by this"){
      val _ = new ExampleSample(s"$moveMethodNotUsedByThis/MovedMethodHasNoParameter.java"){

        val classA = fullName2id("p.A")
        val methToMove = fullName2id("p.A.methodToMove__void")
        val methUserDecl = fullName2id("p.C.user__void")
        val methUserDef = getDefinition(graph, methUserDecl)

        val classB = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)

        assert(graph.uses(methUserDef, classA))
        assert(graph.uses(methUserDef, methToMove))

        val g2 = Move.typeMember(graph, List(methToMove), classB, Some(CreateParameter)).right
        assert(g2.container(methToMove).value == classB)
        assert(g2.uses(methUserDef, methToMove))
      }

    }

    scenario("move method used by this - keep reference with parameter"){
      val _ = new ExampleSample(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java"){

        val classA = fullName2id("p.A")
        val methUserDecl = fullName2id("p.A.mUser__void")
        val methUser = getDefinition(graph, methUserDecl)
        val methToMove = fullName2id("p.A.methodToMove__void")

        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)
        assert(graph.uses(methUser, methToMove))
        graph.parameters(methUserDecl) shouldBe empty

        val g2 = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateParameter)).right
        g2.content(classA).size shouldBe (graph.content(classA).size - 1)
        assert(g2.container(methToMove).value == newHostClass)
        assert(g2.uses(methUser, methToMove))

        val paramList = g2.parameters(methUserDecl)
        paramList.size shouldBe 1
        assert(g2.uses(paramList.head, newHostClass))
      }
    }

    scenario("move method used by this - keep reference with Field"){
      val _ = new ExampleSample(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java"){

        val classA = fullName2id("p.A")
        val methUserDecl = fullName2id("p.A.mUser__void")
        val methUser = getDefinition(graph, methUserDecl)
        val methToMove = fullName2id("p.A.methodToMove__void")

        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)
        assert(graph.uses(methUser, methToMove))
        assert(! graph.uses(methUser, newHostClass))

        val g2 = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateTypeMember(Field))).right
        //quickFrame(g2)
        val ma2Delegate =
          g2.content(classA).find{
            id =>
              g2.getConcreteNode(id).name == "b_delegate"
          }.value

        assert(g2.container(methToMove).value == newHostClass)

        assert(g2.uses(methUser, methToMove))
        assert(g2.uses(ma2Delegate, newHostClass))
        assert(g2.uses(methUser, ma2Delegate))
      }
    }

    scenario("move method used by this - user also via self another method that will not be moved "){
      val _ = new ExampleSample(s"$moveMethodUsedByThis/UserUseAlsoAnotherSelfMethod.java") {

        val classA = fullName2id("p.A")
        val methUserDecl = fullName2id("p.A.mUser__void")
        val methUserDef = getDefinition(graph, methUserDecl)

        val methToMove = fullName2id("p.A.mUsedToMove__void")

        val otherUsedMethod = fullName2id("p.A.mUsedOther__void")

        val newHostClass = fullName2id("p.B")

        assert(graph.uses(methUserDef, methToMove))
        assert(graph.uses(methUserDef, otherUsedMethod))
        graph.typeUsesOf(Uses(methUserDef, methToMove)) should contain (Uses(classA, classA))
        graph.typeUsesOf(Uses(methUserDef, otherUsedMethod)) should contain (Uses(classA, classA))

        val g2 = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateTypeMember(Field))).right

        val ma2Delegate =
          g2.content(classA).find{
            id =>
              g2.getConcreteNode(id).name == "b_delegate"
          }.value


        assert(g2.uses(methUserDef, methToMove))
        assert(g2.uses(methUserDef, otherUsedMethod))

        assert(g2.uses(ma2Delegate, newHostClass))

        g2.typeUsesOf(Uses(methUserDef, methToMove)) should contain (Uses(ma2Delegate, newHostClass))
        println(g2.typeUsesOf(Uses(methUserDef, otherUsedMethod)))
        g2.typeUsesOf(Uses(methUserDef, otherUsedMethod)) should contain (Uses(classA, classA))

      }
    }


    scenario("move method used by this several times - keep reference with Parameter"){


      val _ = new ExampleSample(s"$moveMethodUsedByThis/UsedBySelfSeveralTimes.java"){

        val classA = fullName2id("p.A")
        val methUser1Decl = fullName2id("p.A.mUser1__void")
        val methUser2Decl = fullName2id("p.A.mUser2__void")

        val methUser1 = getDefinition(graph, methUser1Decl)
        val methUser2 = getDefinition(graph, methUser2Decl)

        val methToMove = fullName2id("p.A.methodToMove__void")
        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)

        assert(graph.uses(methUser1, methToMove))
        graph.parameters(methUser1Decl).size shouldBe 0

        assert(graph.uses(methUser2, methToMove))
        graph.parameters(methUser2Decl).size shouldBe 0

        val g2 = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateParameter)).right

        val params1 = g2.parameters(methUser1Decl)
        params1.size shouldBe 1
        val params2 = g2.parameters(methUser2Decl)
        params2.size shouldBe 1

        assert(g2.container(methToMove).value == newHostClass)

        assert(g2.uses(methUser1, methToMove))
        assert(g2.uses(params1.head, newHostClass))

        assert(g2.uses(methUser2, methToMove))
        assert(g2.uses(params2.head, newHostClass))

      }
    }

    scenario("move method used by this several times - keep reference with Field"){
      val _ = new ExampleSample(s"$moveMethodUsedByThis/UsedBySelfSeveralTimes.java"){

        val classA = fullName2id("p.A")
        val methUser1Decl = fullName2id("p.A.mUser1__void")
        val methUser2Decl = fullName2id("p.A.mUser2__void")

        val methUser1 = getDefinition(graph, methUser1Decl)
        val methUser2 = getDefinition(graph, methUser2Decl)

        val methToMove = fullName2id("p.A.methodToMove__void")

        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMove).value == classA)
        assert(graph.uses(methUser1, methToMove))
        assert(graph.uses(methUser2, methToMove))

        val g2 =
          Move.typeMember(graph, List(methToMove), newHostClass,
            Some(CreateTypeMember(Field))).right

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
        graph.styp(methMa).value match {
          case Arrow(in @ Tuple(_), _) =>
            assert(1 == in.length && in.ids.contains(classB))
        }

        assert(graph.container(methMa).value == classA)
        assert(graph.uses(methUser, methMa))

        val g2 = Move.typeMember(graph, List(methMa), classB).right

        assert(g2.container(methMa).value == classB)
        assert(g2.uses(methUser, methMa))

        g2.styp(methMa).value match {
          case Arrow(in @ Tuple(_), _) => assert(0 == in.length)
        }
      }
    }


    scenario("moved method uses this - keep reference with parameter "){
      val _ = new ExampleSample(s"$moveMethod_movedMethodUsesThis/MovedMethodHasNoParameter.java"){

        val currentHost = fullName2id("p.A")
        val methToMoveDecl = fullName2id("p.A.methodToMove__void")
        val methToMoveDef = getDefinition(graph, methToMoveDecl)
        val methUsed = fullName2id("p.A.mUsed__void")

        val newHostClass = fullName2id("p.B")

        assert(graph.container(methToMoveDecl).value == currentHost)
        assert(graph.uses(methToMoveDef, methUsed))
        graph.parameters(methToMoveDecl).size shouldBe 0

        val g2 = Move.typeMember(graph, List(methToMoveDecl), newHostClass, Some(CreateParameter)).right

        g2.content(currentHost).size shouldBe (graph.content(currentHost).size - 1)

        assert(g2.container(methToMoveDecl).value == newHostClass)
        assert(g2.uses(methToMoveDef, methUsed))
        val params = g2.parameters(methToMoveDecl)
        params.size shouldBe 1
        assert(g2.uses(params.head, currentHost))
      }

    }
  }

  feature("Move methods"){

    scenario("one of the moved is used by another") {
      val _ = new ExampleSample(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java") {

        val classA = fullName2id("p.A")
        val methUserDecl = fullName2id("p.A.mUser__void")
        val methUser = getDefinition(graph, methUserDecl)

        val methToMoveDecl = fullName2id("p.A.methodToMove__void")

        val newHostClass = fullName2id("p.B")



        val g2 = Move.typeMember(graph, List(methToMoveDecl, methUserDecl), newHostClass, Some(CreateParameter)).right

        g2.content(classA).size shouldBe (graph.content(classA).size - 2)
        assert(g2.container(methToMoveDecl).value == newHostClass)
        assert(g2.container(methUserDecl).value == newHostClass)
        assert(g2.uses(methUser, methToMoveDecl))
        assert(!g2.uses(methUserDecl, newHostClass))
      }

    }


    scenario("two moved method both used by an unmoved one") {
      val _ = new ExampleSample(s"$moveMethodUsedByThis/TwoMovedMethodUsedBySameUnmovedSibling.java") {

        val classA = fullName2id("p.A")
        val methUserDecl = fullName2id("p.A.mUser__void")
        val methUser = getDefinition(graph, methUserDecl)
        val methToMove1 = fullName2id("p.A.methodToMove1__void")
        val methToMove2 = fullName2id("p.A.methodToMove2__void")

        val newHostClass = fullName2id("p.B")

        val numArgs = graph.parameters(methUserDecl).size

        val g2 = Move.typeMember(graph, List(methToMove1, methToMove2), newHostClass, Some(CreateParameter)).right

        g2.content(classA).size shouldBe (graph.content(classA).size - 2)
        g2.parameters(methUserDecl).size  shouldBe (numArgs + 1)
        assert(g2.container(methToMove1).value == newHostClass)
        assert(g2.container(methToMove2).value == newHostClass)

        assert(g2.uses(methUser, methToMove1))
        assert(g2.uses(methUser, methToMove2))
      }

    }



  }
}
