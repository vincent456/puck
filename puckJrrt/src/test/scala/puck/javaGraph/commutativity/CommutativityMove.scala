package puck.javaGraph.commutativity

import puck.Settings._
import puck.graph.{ShowDG, Factory}
import puck.graph.comparison.Mapping
import puck.graph.transformations.rules.{CreateTypeMember, CreateParameter, Move}
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind.Field
import puck.util.Debug
import puck.{QuickFrame, Settings, AcceptanceSpec}

class CommutativityMove extends AcceptanceSpec {

  val examplesPath = Settings.testExamplesPath + "/move"

  feature("Move class") {

    scenario("Move top level class") {
      val p = "topLevelClass"
      val _ = new ScenarioFactory(s"$examplesPath/$p/A.java",
        s"$examplesPath/$p/Empty.java"){
        val package2 = fullName2id(s"p2")
        val classA = fullName2id(s"p1.A")

        val g = Move.staticDecl(graph, classA, package2).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )

      }
   }

    scenario("Move top from different Packages - local var decl") {
      val p = "topLevelClass/classesInDifferentPackages/localVarDecl"
      val _ = new ScenarioFactory(
        s"$examplesPath/$p/A.java",
        s"$examplesPath/$p/B.java",
        s"$examplesPath/$p/Empty.java") {

        val p1 = fullName2id(s"p1")
        val package3 = fullName2id(s"p3")

        val classA = fullName2id(s"p1.A")

        val g = Move.staticDecl(graph, classA, package3).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        val gClean =
          g.removeContains(g.rootId, p1).removeNode(p1)._2
        assert( Mapping.equals(recompiledEx.graph, gClean) )

      }

    }

    scenario("Move top from different Packages - field decl") {
      val p = "topLevelClass/classesInDifferentPackages/fieldDecl"
      val _ = new ScenarioFactory(
        s"$examplesPath/$p/A.java",
        s"$examplesPath/$p/B.java",
        s"$examplesPath/$p/Empty.java") {

        val p1 = fullName2id(s"p1")

        val package3 = fullName2id(s"p3")

        val classA = fullName2id(s"p1.A")

        val g = Move.staticDecl(graph, classA, package3).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        val gClean =
          g.removeContains(g.rootId, p1).removeNode(p1)._2
        assert( Mapping.equals(recompiledEx.graph, gClean) )

      }

    }

    scenario("Move top from different Packages - with static class member") {
      val d = s"$examplesPath/topLevelClass/3"
      val _ = new ScenarioFactory(
        s"$d/A.java",
        s"$d/B.java",
        s"$d/Empty.java") {

        val p1 = fullName2id("p1")

        val package3 = fullName2id("p3")

        val classA = fullName2id("p1.A")

        val g = Move.staticDecl(graph, classA, package3).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        val gClean =
          g.removeContains(g.rootId, p1).removeNode(p1)._2
        assert( Mapping.equals(recompiledEx.graph, gClean) )

      }

    }
  }


  val moveMethod_movedMethodUsesThis = examplesPath + "/method/movedMethodUsesThis"
  val moveMethodUsedByThis = examplesPath + "/method/usedByThis"
  val movedMethodNOTusedByThis = examplesPath + "/method/NOTusedByThis"


  feature("Move method"){

    scenario("moved method not used by this"){
      val _ = new ScenarioFactory(s"$movedMethodNOTusedByThis/MovedMethodHasNoParameter.java"){

        val methToMove = fullName2id("p.A.methodToMove()")

        val classB = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove), classB, Some(CreateParameter)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }

    }


    scenario("move method used by this - keep reference with parameter"){
      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java"){

        val methToMove = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateParameter)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    scenario("move method used by this - keep reference with Field"){
      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java"){

        val methToMove = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateTypeMember(Field))).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    scenario("move method used by this - user also via self another method that will not be moved "){
      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodUsedAlongASiblingMethodThatIsNotMoved.java") {

        val methToMove = fullName2id("p.A.mUsedToMove()")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateTypeMember(Field))).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }

    scenario("move method used by this several times - keep reference with Parameter"){


      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodUsedByThisSeveralTimes.java"){

        val methToMove = fullName2id("p.A.methodToMove()")
        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateParameter)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    scenario("move method used by this several times - keep reference with Field"){
      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodUsedByThisSeveralTimes.java"){

        val methToMove = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")

        val g =
          Move.typeMember(graph, List(methToMove), newHostClass,
            Some(CreateTypeMember(Field))).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    ignore("Move method not used by this to class of a parameter"){
      val _ = new ScenarioFactory(s"$movedMethodNOTusedByThis/MovedMethodHasNoParameter.java"){

        val methMa = fullName2id("p.A.ma__B")
        val classB = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methMa), classB).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }


    scenario("moved method uses this - keep reference with parameter "){
      val _ = new ScenarioFactory(s"$moveMethod_movedMethodUsesThis/MovedMethodHasNoParameter.java"){

        val methToMoveDecl = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMoveDecl), newHostClass, Some(CreateParameter)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )
      }

    }
  }

  feature("Move methods"){

    scenario("one of the moved method is used by another") {
      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java") {

        val methUserDecl = fullName2id("p.A.mUser()")
        val methToMoveDecl = fullName2id("p.A.methodToMove()")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMoveDecl, methUserDecl), newHostClass, Some(CreateParameter)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )

      }

    }

    scenario("two moved method both used by an unmoved one") {
      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/TwoMovedMethodUsedBySameUnmovedSibling.java") {

        val methToMove1 = fullName2id("p.A.methodToMove1()")
        val methToMove2 = fullName2id("p.A.methodToMove2()")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove1, methToMove2), newHostClass, Some(CreateParameter)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }
  }
  val moveStaticMethod = examplesPath + "/staticMethod"
  feature("Move static method"){
    scenario("unused factory"){
      val _ = new ScenarioFactory(s"$moveStaticMethod/unusedFactory/UnusedFactory.java") {

        val ctor = fullName2id("p.A.A()")
        val factory = fullName2id("p.A.createA()")

        val client = fullName2id("p.Client")

        val g = graph.setRole(factory, Some(Factory(ctor)))

        val g1 = Move.typeMember(g, List(factory), client, None).right

        val recompiledEx = applyChangeAndMakeExample(g1, outDir)
        assert( Mapping.equals(g1, recompiledEx.graph) )

      }
    }

    scenario("used factory moved in client"){
      val _ = new ScenarioFactory(s"$moveStaticMethod/usedFactory/UsedFactory.java") {

        val ctor = fullName2id("p.A.A()")
        val factory = fullName2id("p.A.createA()")

        val client = fullName2id("p.Client")

        val g = graph.setRole(factory, Some(Factory(ctor)))

        val g1 = Move.typeMember(g, List(factory), client, None).right

        val recompiledEx = applyChangeAndMakeExample(g1, outDir)
        assert( Mapping.equals(g1, recompiledEx.graph) )

      }
    }
    scenario("used factory moved in outsider host"){
      val _ = new ScenarioFactory(s"$moveStaticMethod/usedFactory/UsedFactory.java") {

        val ctor = fullName2id("p.A.A()")
        val factory = fullName2id("p.A.createA()")

        val factoryClass = fullName2id("p.Factory")

        val g = graph.setRole(factory, Some(Factory(ctor)))

        val g1 = Move.typeMember(g, List(factory), factoryClass, None).right

        val recompiledEx = applyChangeAndMakeExample(g1, outDir)
        assert( Mapping.equals(g1, recompiledEx.graph) )

      }
    }
  }
}
