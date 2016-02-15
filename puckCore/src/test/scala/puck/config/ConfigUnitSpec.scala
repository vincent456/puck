package puck.config

import java.io.File

import puck.UnitSpec
import puck.util.FileHelper.FileOps
/**
  * Created by lorilan on 12/02/16.
  */
class ConfigUnitSpec extends UnitSpec{

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



  "FileOps" should "compute relative path" in {

    val f1 = new File(tmpDir + "/a/b/c")
    f1.mkdirs()
    val f2 = new File(tmpDir + "/a/b/c/d/e")
    f2.mkdirs()

    f1 pathRelativeTo f2 shouldBe f1.getAbsolutePath
    f2 pathRelativeTo f1 shouldBe "d/e"
  }
 }
