package puck.config

import puck.UnitSpec

/**
  * Created by lorilan on 12/02/16.
  */
class ConfigUnitSpec extends UnitSpec{

  "ConfigWriter" should "write correctly the config" in {
    val confStr = ConfigWriter.show(Config.defautlConfig)
    confStr should startWith ("<puck-config>\n")
    confStr should include ("\t<workspace>.</workspace>\n")
    confStr should include ("\t<src rec=\".java\">src</src>\n")
    confStr should include ("\t<out>out</out>\n")
    confStr should include ("\t<classpath rec=\".jar\">lib</classpath>\n")
    confStr should include ("\t<decouple>decouple.wld</decouple>\n")
    confStr should include ("\t<log>puck-log.txt</log>\n")
    confStr should endWith ("\n</puck-config>")
  }
}
