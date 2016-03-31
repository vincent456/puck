/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.jastadd

import java.io.{File, InputStream}

import puck.graph.comparison.NodeMappingInitialState
import puck.graph.transformations.{Recording, Transformation}
import puck.graph.{DGBuildingError, DependencyGraph, NodeId}
import puck.javaGraph.nodeKind.JavaNodeKind
import org.extendj.ast.{List => _, _} //hide ast.List and import everything else
import org.extendj.parser


object CompileHelper {

  def apply(sources: List[String],
            sourcepaths:List[String],
            jars: List[String],
            bootJars : List[String]): Option[Program] = {
      val arglist = createArglist(sources, sourcepaths, jars, bootJars)
      val f = new Frontend {
//        protected override def processErrors(errors: java.util.Collection[Problem], unit: CompilationUnit): Unit =  {
//          System.err.println("Errors:")
//
//            val it: Iterator[_] = errors.iterator
//            while (it.hasNext) {
//              val i = it.next()
//              System.err.println(i)
//            }
//
//        }
        protected override def processWarnings(errors: java.util.Collection[Problem], unit: CompilationUnit): Unit = {
        }
      }
      val br = new BytecodeReader() {
        def read(is: InputStream, fullName: String, p: Program) : CompilationUnit = {
          new BytecodeParser(is, fullName).parse(null, null, p)
        }
      }

      val jp = new JavaParser() {
        override def parse(is: InputStream, fileName: String) : CompilationUnit = {
          new parser.JavaParser().parse(is, fileName)
        }
      }

    if (f.run(arglist, br, jp) == 0){
      Some(f.getProgram)}
    else
      None
  }

  def buildGraph(p : Program,
                 ll : puck.LoadingListener = null )  :
  (Program, DependencyGraph, Seq[Transformation], Map[String, NodeId], Map[NodeId, ASTNodeLink]) = {
    val builder = p.buildDependencyGraph(ll)
    builder.addContains(builder.nodesByName("@primitive"), builder.arrayTypeId)
    builder.attachOrphanNodes()
    builder.registerSuperTypes()

    val (_, initialRecord) = NodeMappingInitialState.normalizeNodeTransfos(JavaNodeKind.root.kind,
        builder.g.recording, Seq())

    val g = builder.g.newGraph(recording = Recording())

    (p, g,
      initialRecord,
      builder.nodesByName,
      builder.graph2ASTMap)
  }

  def compileSrcsAndbuildGraph(sources: List[String],
                               sourcepaths:List[String],
                               jars: List[String],
                               bootJars : List[String]) :
    (Program, DependencyGraph, Seq[Transformation], Map[String, NodeId], Map[NodeId, ASTNodeLink]) =
    this.apply(sources, sourcepaths, jars, bootJars) match {
      case None => throw new DGBuildingError("Compilation error, no AST generated")
      case Some(p) => buildGraph(p)
    }



  private[puck] def createArglist(sources: List[String],
                                  sourcepaths:List[String],
                                  jars: List[String],
                                  bootClassPath : List[String]): Array[String] = {

    def prepend(argName : String, argValue : List[String], accu : List[String]) : List[String] =
      if(argValue.isEmpty) accu
      else argName :: argValue.mkString(File.pathSeparator) :: accu

    val args0 = prepend("-classpath", jars, sources)
    val args1 = prepend("-sourcepath", sourcepaths, args0)
    prepend("-bootclasspath", bootClassPath, args1).toArray
  }


}
