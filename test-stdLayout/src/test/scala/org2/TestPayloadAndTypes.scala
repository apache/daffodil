package org2

import org.junit.Test
import edu.illinois.ncsa.daffodil.tdml.Runner
import org.junit.AfterClass

object TestPayloadAndTypes {
  val runner = Runner("org2", "testPayloadAndTypes.tdml")

  @AfterClass def shutDown {
    runner.reset
  }
}

class TestPayloadAndTypes {

  import TestPayloadAndTypes._

  @Test def test_data_01() = { runner.runOneTest("data_01") }

}
