package puck

import java.io.File

import org.extendj.ast.JavaJastAddDG2AST
import puck.config.{Config, ConfigParser}
import puck.javaGraph.JGraphUtils

/**
  * Created by lorilan on 11/02/16.
  */
package object jastadd {

  def JavaProject() : Project = JavaProject(new File("."))
  def JavaProject(f : File) : Project  =
    new Project(ConfigParser(Config.defaultConfFile(f)), JavaJastAddDG2AST)

  object ExtendJGraphUtils extends JGraphUtils {
    val dg2astBuilder: DG2ASTBuilder = JavaJastAddDG2AST
  }
}

