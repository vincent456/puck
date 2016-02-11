package puck

import java.io.File

import puck.graph.io.Project.Default
import puck.graph.io.{ConfigParser, DG2ASTBuilder, Project}
import puck.javaGraph.JGraphUtils

/**
  * Created by lorilan on 11/02/16.
  */
package object jastadd {

  import util.FileHelper.FileOps

  def JavaProject() : Project = JavaProject(new File("."))
  def JavaProject(f : File) : Project  =
    new Project(ConfigParser(f \ Default.configFile), JavaJastAddDG2AST)

  object ExtendJGraphUtils extends JGraphUtils {
    val dg2astBuilder: DG2ASTBuilder = JavaJastAddDG2AST
  }
}

