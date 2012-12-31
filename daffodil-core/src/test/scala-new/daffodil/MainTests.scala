package daffodil

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import org.junit.contrib.java.lang.system.ExpectedSystemExit
import org.junit.contrib.java.lang.system.internal.CheckExitCalled
import org.junit.Test
import org.junit.Rule
import daffodil.util.Misc
import java.io.ByteArrayInputStream


class MainTests extends JUnitSuite {

  // This rule makes it so that System.exit throws an exception You are
  // supposed to be able to say exit.expectExitWithStatus(), but this doesn't
  // work, probably some scala problem, related to def exit. This is supposed
  // to be val exit, but it doesn't work with scala. Instead, we need to
  // manually catch CheckExitCalled and verify the status
  @Rule
  def exit = ExpectedSystemExit.none()

  @Test def test_required_mode() {
    try {
      daffodil.Main.main(Array(""))
    } catch {
      case c: CheckExitCalled => assertEquals(c.getStatus, 1)
    }
  }
  
  @Test def test_require_schema_or_parser() {
    try {
      daffodil.Main.main(Array("parse"))
    } catch {
      case c: CheckExitCalled => assertEquals(c.getStatus, 1)
    }
  }


  val testDir = "/test/cli/"
  val cli_choice= testDir + "choice.xsd"

  @Test def test_main_parse_simple() {
    try {
      val schema = Misc.getRequiredResource(cli_choice).getPath
      val data = "1;2;3;4;5;6;7;8"
      val is = new ByteArrayInputStream(data.getBytes())
      val oldSysin = System.in
      System.setIn(is)

      daffodil.Main.main(Array("parse", "--schema", schema, "-"))

      System.setIn(oldSysin)
    } catch {
      case c: CheckExitCalled => assertEquals(c.getStatus, 0)
    }
  }
  
  @Test def test_main_parse_all_opts() {
    try {
      val schema = Misc.getRequiredResource(cli_choice).getPath
      val data = "1;2;3;4.4;5;6;7;8"
      val is = new ByteArrayInputStream(data.getBytes())
      val oldSysin = System.in
      System.setIn(is)
       
      daffodil.Main.main(Array("parse", "--schema", schema,
                                        "--root", "ch1",
                                        "--namespace", "http://www.example.org/example1/",
                                        "-"
                                        ))
      System.setIn(oldSysin)
    } catch {
      case c: CheckExitCalled => assertEquals(c.getStatus, 0)
    }
  }

}
