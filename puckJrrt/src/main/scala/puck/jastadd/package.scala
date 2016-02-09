package puck

import java.io.File

import puck.graph.io.Project

/**
 * Created by lorilan on 03/11/15.
 */
package object jastadd {

  def JavaFilesHandler(root : String) : Project = JavaFilesHandler(new File(root))
  def JavaFilesHandler() : Project = JavaFilesHandler(new File("."))

  def JavaFilesHandler(workingDirectory : java.io.File) : Project =
    new Project(workingDirectory, ".java", JavaJastAddDG2AST)

}
