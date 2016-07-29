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

import java.io.File
import org.extendj.ast
import org.extendj.ast.JavaJastAddDG2AST
import puck.config.Config.Config
import puck.config.{Config, ConfigParser}
import puck.javaGraph.JGraphUtils

package object jastadd {

  object JavaProject {
    def apply() : Project = JavaProject(new File("."))
    def apply(root : String) : Project  = JavaProject(new File(root))
    def apply(root : File) : Project  = withConfig(Config.defaultConfFile(root))

    def withConfig(configFile : String) : Project = withConfig(new File(configFile))

    def withConfig(configFile : File) : Project = withConfig(ConfigParser(configFile))

    def withConfig(cfg : Config) : Project = new Project(cfg, JavaJastAddDG2AST)
  }

//  implicit class JastaddConversion[T <: ast.ASTNode[_]](val i : Iterable[T]) extends AnyVal {
//
//    def toASTList : ast.List[T]=
//      new ast.List[T](i.toSeq:_*)
//
//  }


  object ExtendJGraphUtils extends JGraphUtils {
    val dg2astBuilder: DG2ASTBuilder = JavaJastAddDG2AST
  }
}

