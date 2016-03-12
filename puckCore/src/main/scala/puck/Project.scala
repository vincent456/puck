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

package puck

import java.io._
import puck.config.{Config, ConfigParser}
import puck.graph._
import puck.graph.constraints.{ConstraintsMaps, ConstraintsParser}
import puck.graph.transformations.Transformation
import puck.graph.{DependencyGraph, GraphBuilder}
import puck.util._

object Project{

  def apply(seed : File, dG2ASTBuilder: DG2ASTBuilder) : Project =
    new Project(ConfigParser(seed), dG2ASTBuilder)
}

trait DG2ASTBuilder{
  def apply(fh : Project,
            logger : PuckLogger,
            ll : LoadingListener = null) : DG2AST
}

trait DG2AST {
  def apply(graph : DependencyGraph)(implicit logger : PuckLogger) : Unit
  def printCode(dir : File)(implicit logger : PuckLogger) : Unit
  def initialGraph : DependencyGraph
  def initialRecord : Seq[Transformation]
  def nodesByName : Map[String, NodeId]
  def code(graph : DependencyGraph, id : NodeId) : String
}
import Config._
class Project
(var config : Config,
 val dG2ASTBuilder: DG2ASTBuilder){



  def apply[T](key : ConfigKey[T])  : Option[T] = config get key

  def set(k : FileKey, f : SingleFile) : Unit =
    config = config put (k, f)

  def add(k : FileListKey, ff : FileFinder) : Unit = {
    val prev = config getOrElse (k, List())

    config = config put (k, ff :: prev)
  }
  def remove(k : FileListKey, ff : FileFinder) : Unit = {
    val prev = config getOrElse (k, List())

    config = config put (k, prev filter (_ != ff))
  }


  def pathList(k : FileListKey ) : List[String] = {
    val l = config getOrElse (k, List())
    l.foldLeft(List[String]()){
      case (acc, f @ SingleFile(_)) => f.resolvePath(workspace) :: acc
      case (acc,f @ Root(_, suffix, excludes)) =>
        import puck.util.FileHelper.{findAllFiles, FileOps}
        val resolvedPath =  new File(f.resolvePath(workspace))
        val resolvedExcludes = excludes map (resolvedPath \ _)
        findAllFiles(suffix, resolvedExcludes, acc, resolvedPath)

    }
  }


  def workspace : File =
    new File( (config get Keys.workspace getOrElse SingleFile(".")).path )

  def someFile(k : FileKey) : Option[File] =
    config get k map (v => new File(v.resolvePath(workspace)))


  def fromOutDir : Project =
    new Project(config, dG2ASTBuilder)


  import PuckLog.defaultVerbosity

  type GraphT = DependencyGraph

  var graphBuilder : GraphBuilder = _


  def decouple = someFile(Keys.decouple)

  def outDirectory = someFile(Keys.out)

  def graphvizDot = someFile(Keys.dotPath)

  def editor = someFile(Keys.editor)

  def logFile = someFile(Keys.log)



  def loadGraph
  ( ll : Option[LoadingListener] = None)
  ( implicit logger : PuckLogger) : DG2AST = {
     dG2ASTBuilder(this, logger, ll.orNull)
  }

  def parseConstraints
  ( dg2ast: DG2AST )
  ( implicit logger : PuckLogger) : Option[ConstraintsMaps] = {
    decouple match{
      case None =>
        logger.writeln("cannot parse : no decouple file given")((PuckLog.NoSpecialContext, PuckLog.Error))
        None
      case Some(f) =>
        logger.writeln("parsing " + f)
        try Some(ConstraintsParser(dg2ast.nodesByName, new FileReader(f)))
        catch {
          case t : Throwable =>
            logger.writeln("parsing failed : " + t.getMessage)((PuckLog.NoSpecialContext, PuckLog.Error))
            None
        }
    }
  }

//  private def openList(files : Seq[String]) : Unit = {
//    val ed = editor.get match {
//      case None => sys.env("EDITOR")
//      case Some(f) => f.getCanonicalPath
//    }
//    Process(ed  +: files ).!;()
//  }
//
//  import puck.util.FileHelper.findAllFiles
//
//  def openSources() = openList(findAllFiles(srcDirectory !, srcSuffix,
//    outDirectory.toOption map (_.getName)))
//  def openProduction() = openList(findAllFiles(outDirectory !, srcSuffix,
//    outDirectory.toOption map (_.getName)))

}