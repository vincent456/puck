package puck

import java.io.File

import puck.graph.io.FilesHandler

/**
 * Created by lorilan on 03/11/15.
 */
package object jastadd {

  def JavaFilesHandler(root : String) : FilesHandler = JavaFilesHandler(new File(root))
  def JavaFilesHandler() : FilesHandler = JavaFilesHandler(new File("."))

  def JavaFilesHandler(workingDirectory : java.io.File) : FilesHandler =
    new FilesHandler(workingDirectory, ".java", JavaJastAddDG2AST)

}
