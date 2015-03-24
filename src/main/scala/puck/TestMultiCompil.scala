package puck

import puck.javaGraph.CompileHelper

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by lorilan on 3/24/15.
 */
object TestMultiCompil {

    def compile(fileName : String) =
      CompileHelper(List(fileName), List())

    def main(args :Array[String]): Unit = {
      val testPath = "/home/lorilan/projects/constraintsSolver/src/test"
      val testExamplesPath = testPath + "/resources/examples"
      val typeDeclPath = testExamplesPath + "/redirection/typeDecl/"

      for (_ <- 1 until 100) {
        Future {
          compile(s"$typeDeclPath/classToInterfaceSuperType/A.java")
        }
      }
    }
}
