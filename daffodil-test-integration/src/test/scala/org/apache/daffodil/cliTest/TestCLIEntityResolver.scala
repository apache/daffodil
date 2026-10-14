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
import org.apache.daffodil.cli.cliTest.Util.*
import org.apache.daffodil.entityresolver.LoggingEntityResolver

import org.junit.Test

class TestCLIEntityResolver {

  /**
   * Return a sequence of paths, made up of the one classpath containing the compiled
   * test resolver factories, and the directory containing the META-INF dir that
   * registers them.
   */
  private def resolverClasspath(extra: String*): Seq[Path] = {
    val classes = path(s"daffodil-core/target/$scalaVersionForTargetPath/test-classes/")
    classes +: extra.map(path(_))
  }

  private val schema = path(
    "daffodil-cli/src/test/resources/org/apache/daffodil/cli/cli_schema.dfdl.xsd"
  )

  /**
   * Without a registered factory the default resolver is used and parsing works.
   */
  @Test def test_noFactory_usesDefault(): Unit = {
    runCLI(args"parse -s $schema -r e", resolverClasspath(), fork = true) { cli =>
      cli.send("5/42,2", inputDone = true)
      cli.expect("<g>2</g>")
    }(ExitCode.Success)
  }

  /**
   * A registered factory supplies the resolver that schema loading uses.
   */
  @Test def test_singleFactory_isUsed(): Unit = {
    val classpath = resolverClasspath(
      "daffodil-core/src/test/resources/org/apache/daffodil/entityresolver/single/"
    )
    runCLI(args"parse -s $schema -r e", classpath, fork = true) { cli =>
      cli.send("5/42,2", inputDone = true)
      cli.expectErr(LoggingEntityResolver.marker)
      cli.expect("<g>2</g>")
    }(ExitCode.Success)
  }

  /**
   * More than one registered factory is an error.
   */
  @Test def test_twoFactories_isAnError(): Unit = {
    val classpath = resolverClasspath(
      "daffodil-core/src/test/resources/org/apache/daffodil/entityresolver/double/"
    )
    runCLI(args"parse -s $schema -r e", classpath, fork = true) { cli =>
      cli.send("5/42,2", inputDone = true)
      cli.expectErr("At most one DaffodilEntityResolverFactory may be registered")
    }(ExitCode.BugFound)
  }
}
