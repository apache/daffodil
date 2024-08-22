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

package org.apache.daffodil.cliTest

import java.nio.file.Path

import org.apache.daffodil.cli.Main.ExitCode
import org.apache.daffodil.cli.cliTest.Util._

import org.junit.Test

class TestCLIPlugins {

  /**
   * Return a sequence of paths, made up of the one classpath containing all
   * compiled daffodil-test .class files, and any additional classpaths needed for a
   * specific test
   */
  private def testClasspath(extra: String*): Seq[Path] = {
    val classes = path("daffodil-test/target/scala-2.12/test-classes/")
    val paths = extra.map(path(_))
    classes +: paths
  }

  /**
   * Tests the case when reloading a saved parser when a needed charset is not on the classpath.
   */
  @Test def test_reload_missing_charset(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/charsets/TestBitsCharsetDefinition.dfdl.xsd"
    )
    val classpath = testClasspath()

    withTempFile { parser =>
      // create a saved parser with the correct classpath
      runCLI(args"save-parser -s $schema -r s2 $parser", classpath, fork = true) { cli => }(
        ExitCode.Success
      )

      // exclude classpath so reloading is missing the charset class
      runCLI(args"parse -P $parser", fork = true) { cli =>
        cli.expectErr(
          "Charset plugin org.apache.daffodil.charsets.BitsCharsetTest_ISO_8859_13$ for ISO-8859-13"
        )
      }(ExitCode.UnableToCreateProcessor)

      // make sure it works with the correct classpath
      runCLI(args"parse -P $parser", classpath, fork = true) { cli =>
        val bytes = Array[Int](
          0xc0, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0xc0, 0x31, 0x32, 0x33, 0x34, 0x35,
          0x36, 0x37
        ).map(_.toByte)
        cli.sendBytes(bytes, inputDone = true)
        cli.expect("<e1>À1234567</e1>")
        cli.expect("<e2>Ą1234567</e2>")
      }(ExitCode.Success)
    }
  }

  /**
   * Tests the case when reloading a saved parser when a needed layer is not on the classpath.
   */
  @Test def test_reload_missing_layer(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/layers/useAllTypesLayer.dfdl.xsd"
    )
    val classpath = testClasspath()

    withTempFile { parser =>
      // create a saved parser with the correct classpath
      runCLI(args"save-parser -s $schema -r root $parser", classpath, fork = true) { cli => }(
        ExitCode.Success
      )

      // exclude classpath so reloading is missing the charset class
      runCLI(args"parse -P $parser", fork = true) { cli =>
        cli.expectErr("{urn:org.apache.daffodil.layers.xsd.AllTypesLayer}allTypesLayer")
      }(ExitCode.UnableToCreateProcessor)

      // make sure it works with the correct classpath
      runCLI(args"parse -P $parser", classpath, fork = true) { cli =>
        cli.send("", inputDone = true)
        cli.expect("<integer2>-84</integer2>")
      }(ExitCode.Success)
    }
  }

  /**
   * Tests the case when reloading a saved parser when a needed udf is not on the classpath.
   */
  @Test def test_reload_missing_udf(): Unit = {
    val schema = path(
      "daffodil-udf/src/test/resources/org/apache/daffodil/udf/genericUdfSchema.xsd"
    )
    val classpath = testClasspath(
      "daffodil-udf/target/scala-2.12/test-classes/"
    )

    withTempFile { parser =>
      // create a saved parser with the correct classpath
      runCLI(args"save-parser -s $schema -r user_func1 $parser", classpath, fork = true) {
        cli =>
      }(ExitCode.Success)

      // exclude classpath so reloading is missing the charset class
      runCLI(args"parse -P $parser", fork = true) { cli =>
        cli.expectErr("org.jgoodudfs.example.StringFunctions.Replace")
      }(ExitCode.UnableToCreateProcessor)

      // make sure it works with the correct classpath
      runCLI(args"parse -P $parser", classpath, fork = true) { cli =>
        cli.send("strng", inputDone = true)
        cli.expect("<user_func1>")
      }(ExitCode.Success)
    }
  }

}
