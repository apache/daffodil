/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.cli.cliTest

import java.nio.file.Files

import org.apache.daffodil.cli.Main.ExitCode
import org.apache.daffodil.cli.cliTest.Util.*

import org.junit.Assert.assertEquals
//import org.junit.Test

class TestCLIMisc {

  /**
   * Create a saved parser for the same schema but in two different directories, the resulting
   * saved parsers should be exactly the same
   *
   * Note that this test is disabled because JVM optimizations (e.g. serializing zero-length
   * arrays to singletons) are somewhat inconsistently applied and can lead to saved parsers
   * that are functionally the same but not bit-for-bit identical, causing CI to randomly fail.
   * This should be enabled when DAFFODIL-2925 is fixed.
   */
  /*@Test*/
  def test_CLI_SaveParser_reproducible(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val schemaName = schema.getFileName

    val md5sums: Seq[String] = (1 to 2).map { _ =>
      withTempDir { dir =>
        withTempFile { parser =>
          val newSchema = dir.resolve(schemaName)
          Files.copy(schema, newSchema)
          runCLI(args"save-parser -s $schemaName $parser", directory = Some(dir)) { cli => }(
            ExitCode.Success
          )
          Util.md5sum(parser)
        }
      }
    }

    assertEquals(md5sums(0), md5sums(1))
  }
}
