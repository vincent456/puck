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

package puck.config

import java.io.File

import puck.UnitSpec
import puck.util.FileHelper.FileOps
/**
  * Created by Loïc Girault on 12/02/16.
  */
class UtilsUnitSpec extends UnitSpec{

  "ConfigWriter" should "write correctly the config" in {
    val f = new File(".")
    val confStr = ConfigWriter.show(Config.defautlConfig(f))
    confStr should startWith ("<puck-config>\n")
    confStr should include (s"\t<workspace>${f.getAbsolutePath}</workspace>\n")
    confStr should include ("\t<src rec=\".java\">src</src>\n")
    confStr should include ("\t<out>out</out>\n")
    confStr should include ("\t<classpath rec=\".jar\">lib</classpath>\n")
    confStr should include ("\t<decouple>decouple.wld</decouple>\n")
    confStr should include ("\t<log>puck-log.txt</log>\n")
    confStr should endWith ("\n</puck-config>")
  }

  val tmpDir = System.getProperty("java.io.tmpdir")

  "File with same absolut path" should "be equals" in {
    val f1 = new File(tmpDir + "/a/b")
    val f2 = new File(tmpDir + "/a/b")

    assert(f1 == f2)
  }

  "FileOps" should "compute ancestors" in {
    val f = new File(tmpDir + "/a/b/c")
    val root = new File(tmpDir )
    val a = new File(tmpDir + "/a")
    val b = new File(tmpDir + "/a/b")

    val ancestors = f.ancestors
    ancestors should contain (root)
    ancestors should contain (a)
    ancestors should contain (b)

  }



  it should "compute relative path" in {

    val f1 = new File(tmpDir + "/a/b/c")
    f1.mkdirs()
    val f2 = new File(tmpDir + "/a/b/c/d/e")
    f2.mkdirs()


    f1 pathRelativeTo f2 shouldBe f1.getAbsolutePath
    f2 pathRelativeTo f1 shouldBe ("d" + File.separator + "e")
  }
 }
