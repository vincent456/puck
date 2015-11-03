package puck.javaGraph.commutativity

import puck.Settings._
import puck.graph.comparison.Mapping
import puck.graph.transformations.rules.{CreateTypeMember, CreateParameter, Move}
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind.Field
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
  }

  val moveMethod_movedMethodUsesThis = examplesPath + "/method/movedMethodUsesThis"
  val moveMethodUsedByThis = examplesPath + "/method/usedByThis"
  val movedMethodNOTusedByThis = examplesPath + "/method/NOTusedByThis"


  feature("Move method"){

    scenario("moved method not used by this"){
      val _ = new ScenarioFactory(s"$movedMethodNOTusedByThis/MovedMethodHasNoParameter.java"){

        val methToMove = fullName2id("p.A.methodToMove__void")

        val classB = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove), classB, Some(CreateParameter)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )
      }

    }


    scenario("move method used by this - keep reference with parameter"){
      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java"){

        val methToMove = fullName2id("p.A.methodToMove__void")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateParameter)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    scenario("move method used by this - keep reference with Field"){
      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodHasNoParameter.java"){

        val methToMove = fullName2id("p.A.methodToMove__void")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateTypeMember(Field))).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

//        import scalaz.syntax.show._
//        import puck.util.Debug.showNodeIndex
//        g.nodesIndex.println
//        recompiledEx.graph.nodesIndex.println
//
//        val mapping = Mapping.create(g, recompiledEx.graph)
//
//       println(Mapping.mapCVM(mapping, g.edges.userMap))
//       println(recompiledEx.graph.edges.userMap)

        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    scenario("move method used by this - user also via self another method that will not be moved "){
      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodUsedAlongASiblingMethodThatIsNotMoved.java") {

        val methToMove = fullName2id("p.A.mUsedToMove__void")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateTypeMember(Field))).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )
      }
    }

    scenario("move method used by this several times - keep reference with Parameter"){


      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodUsedByThisSeveralTimes.java"){

        val methToMove = fullName2id("p.A.methodToMove__void")
        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove), newHostClass, Some(CreateParameter)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }

    scenario("move method used by this several times - keep reference with Field"){
      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/MovedMethodUsedByThisSeveralTimes.java"){

        val methToMove = fullName2id("p.A.methodToMove__void")

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

        val methToMoveDecl = fullName2id("p.A.methodToMove__void")

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

        val methUserDecl = fullName2id("p.A.mUser__void")
        val methToMoveDecl = fullName2id("p.A.methodToMove__void")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMoveDecl, methUserDecl), newHostClass, Some(CreateParameter)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)

        assert( Mapping.equals(g, recompiledEx.graph) )

      }

    }

    scenario("two moved method both used by an unmoved one") {
      val _ = new ScenarioFactory(s"$moveMethodUsedByThis/TwoMovedMethodUsedBySameUnmovedSibling.java") {

        val methToMove1 = fullName2id("p.A.methodToMove1__void")
        val methToMove2 = fullName2id("p.A.methodToMove2__void")

        val newHostClass = fullName2id("p.B")

        val g = Move.typeMember(graph, List(methToMove1, methToMove2), newHostClass, Some(CreateParameter)).right

        val recompiledEx = applyChangeAndMakeExample(g, outDir)
        assert( Mapping.equals(g, recompiledEx.graph) )

      }
    }
  }
}
