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

import java.nio.file.Files
import java.nio.file.Path

import org.apache.daffodil.cli.Main.ExitCode
import org.apache.daffodil.cli.cliTest.Util.*
import org.apache.daffodil.entityresolver.LoggingLSResourceResolver
import org.apache.daffodil.entityresolver.LoggingURIResolver
import org.apache.daffodil.entityresolver.LoggingXMLEntityResolver

import org.junit.Test

class TestCLIEntityResolver {

  /**
   * Return a sequence of paths, made up of the one classpath containing the compiled
   * test resolver providers, and the directory containing the META-INF dir that
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
   * Without a registered provider the default resolver is used and parsing works.
   */
  @Test def test_noProvider_usesDefault(): Unit = {
    runCLI(args"parse -s $schema -r e", resolverClasspath(), fork = true) { cli =>
      cli.send("5/42,2", inputDone = true)
      cli.expect("<g>2</g>")
    }(ExitCode.Success)
  }

  /**
   * A registered provider supplies the resolver that schema loading uses.
   */
  @Test def test_singleProvider_isUsed(): Unit = {
    val classpath = resolverClasspath(
      "daffodil-core/src/test/resources/org/apache/daffodil/entityresolver/single/"
    )
    runCLI(args"parse -s $schema -r e", classpath, fork = true) { cli =>
      cli.send("5/42,2", inputDone = true)
      cli.expectErr(LoggingLSResourceResolver.marker)
      cli.expect("<g>2</g>")
    }(ExitCode.Success)
  }

  private val providers = "daffodil-core/src/test/resources/org/apache/daffodil/entityresolver/"

  /**
   * Schema-informed EXI builds its grammars with the provider's XML entity resolver.
   */
  @Test def test_exiGrammars_useTheProvidersXMLResolver(): Unit = {
    val exiSchema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd"
    )
    withTempFile { out =>
      runCLI(
        args"parse -I exisa -s $exiSchema -r e1 -o $out",
        resolverClasspath(providers + "single/"),
        fork = true
      ) { cli =>
        cli.send("Hello", inputDone = true)
        cli.expectErr(LoggingXMLEntityResolver.marker)
      }(ExitCode.Success)
    }
  }

  /**
   * Schematron validation builds its transformer with the provider's URI resolver.
   */
  @Test def test_schematron_usesTheProvidersURIResolver(): Unit = {
    val schematronSchema = path("daffodil-schematron/src/test/resources/xsd/string.dfdl.xsd")
    val rules = path("daffodil-schematron/src/test/resources/sch/never-fails.sch")
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/uuid.txt")
    runCLI(
      args"parse --validate schematron=$rules -s $schematronSchema $input",
      resolverClasspath(providers + "single/"),
      fork = true
    ) { cli =>
      cli.expect("<never-fails>")
      cli.expectErr(LoggingURIResolver.marker)
    }(ExitCode.Success)
  }

  /**
   * Daffodil resolves its own namespaces itself, so a provider that fails if it is asked for
   * one still works.
   */
  @Test def test_providerIsNeverAskedForBuiltInNamespaces(): Unit = {
    runCLI(
      args"parse -s $schema -r e",
      resolverClasspath(providers + "strict/"),
      fork = true
    ) { cli =>
      cli.send("5/42,2", inputDone = true)
      cli.expect("<g>2</g>")
    }(ExitCode.Success)
  }

  /**
   * Catalogs configured with CatalogManager.properties still apply when a provider is registered.
   */
  @Test def test_userCatalogs_stillApplyWithAProvider(): Unit = {
    val main = path(providers + "usercatalog/main.dfdl.xsd")
    val catalog = path(providers + "usercatalog/user-catalog.xml").toAbsolutePath
    withTempDir { dir =>
      Files.writeString(dir.resolve("CatalogManager.properties"), s"catalogs=$catalog\n")
      runCLI(
        args"parse -s $main -r root",
        resolverClasspath(providers + "single/") :+ dir,
        fork = true
      ) { cli =>
        cli.send("abc", inputDone = true)
        cli.expect("<v>abc</v>")
      }(ExitCode.Success)
    }
  }

  /**
   * If no provider is left, because one shares the default's name and both are ignored, that is
   * an error.
   */
  @Test def test_noProviderLeft_isAnError(): Unit = {
    val classpath = resolverClasspath(providers + "nodefault/")
    runCLI(args"parse -s $schema -r e", classpath, fork = true) { cli =>
      cli.send("5/42,2", inputDone = true)
      cli.expectErr("No DaffodilEntityResolverProvider found")
    }(ExitCode.BugFound)
  }

  /**
   * More than one registered provider is an error.
   */
  @Test def test_twoProviders_isAnError(): Unit = {
    val classpath = resolverClasspath(
      "daffodil-core/src/test/resources/org/apache/daffodil/entityresolver/double/"
    )
    runCLI(args"parse -s $schema -r e", classpath, fork = true) { cli =>
      cli.send("5/42,2", inputDone = true)
      cli.expectErr("At most one DaffodilEntityResolverProvider may be registered")
    }(ExitCode.BugFound)
  }
}
