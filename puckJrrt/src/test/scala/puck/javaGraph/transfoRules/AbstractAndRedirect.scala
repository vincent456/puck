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

package puck.javaGraph.transfoRules

import puck.AcceptanceSpec
import puck.graph.constraints.SupertypeAbstraction
import puck.graph._
import puck.jastadd.ExtendJGraphUtils.{Rules => Rules}
import puck.javaGraph.ScenarioFactory
import puck.javaGraph.nodeKind.Interface

/**
  * Created by Loïc Girault on 20/04/16.
  */
class AbstractAndRedirect extends AcceptanceSpec {

  scenario("abstract File and redirect toward abstraction") {
    val _ = new ScenarioFactory(
      """package fileSystem;
        |
        |public class File {
        |   public void display(String path){ System.out.println(path+name); }
        |   private String name;
        |}""",
      """package fileSystem;
        |
        |import java.util.ArrayList;
        |import java.util.List;
        |
        |public class Directory {
        |   public void display(String path) {
        |      System.out.println(path + name);
        |      String npath = path + name +"/";
        |      for(File f: files)
        |         f.display(npath);
        |      for(Directory d: directories)
        |         d.display(npath);
        |   }
        |   private String    name;
        |   private List<File> files = new ArrayList<File>();
        |   private List<Directory> directories = new ArrayList<Directory>();
        |}"""
    ) {

      val g0 = graph

      def abstractFile(g : DependencyGraph) : (NodeId, DependencyGraph) = {
        val (AccessAbstraction(itcId, _), g1) =
          Rules.abstracter.createAbstraction(g, g getConcreteNode "fileSystem.File",
            Interface, SupertypeAbstraction).rvalue

        val g2 = g1.addContains("fileSystem", itcId)

        (itcId, Rules.rename(g2, itcId, "FSElement"))
      }

      val (fsElement, g1) = abstractFile(g0)

      val g2 = Rules.redirection.redirectUsesAndPropagate(g1,
        Uses("fileSystem.Directory.files", "fileSystem.File"),
        AccessAbstraction(fsElement, SupertypeAbstraction)).rvalue

      assert(g2.uses("fileSystem.Directory.display(String).Definition", "fileSystem.Directory"))
      assert(g2.uses("fileSystem.Directory.display(String).Definition", (g2, "fileSystem.FSElement")))
    }
  }
}
