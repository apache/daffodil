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

package org.apache.daffodil.cli.cliTest.schematron

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption.APPEND
import scala.xml.Elem
import scala.xml.XML

import org.apache.daffodil.cli.Main.ExitCode
import org.apache.daffodil.cli.cliTest.Util._

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class TestSvrlOutput {

  private def makeConf(conf: Path, schematron: Path, svrl: Path): Unit = {
    Files.write(
      conf,
      s"""schematron.path=${jsonEscape(schematron.toString)}\n""".getBytes(UTF_8),
      APPEND
    )
    Files.write(
      conf,
      s"""schematron.svrl.file=${jsonEscape(svrl.toString)}\n""".getBytes(UTF_8),
      APPEND
    )
  }

  @Test def validationSuccess(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/string.dfdl.xsd")
    val schematron = path("daffodil-schematron/src/test/resources/sch/never-fails.sch")
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/uuid.txt")

    withTempFile(
      ".conf",
      { conf =>
        withTempFile { svrl =>
          makeConf(conf, schematron, svrl)

          runCLI(args"parse --validate schematron=$conf -s $schema $input") { cli =>
            cli.expect("<never-fails>2f6481e6-542c-11eb-ae93-0242ac130002</never-fails>")
          }(ExitCode.Success)

          XML.loadFile(svrl.toFile) match {
            case Elem("svrl", "schematron-output", _, _, rules @ _*) =>
              val res = rules.find { r =>
                r match {
                  case Elem("svrl", "failed-assert", _, _, _*) => true
                  case _ => false
                }
              }
              // we should not have found failures
              assertFalse(res.isDefined)
            case _ =>
              fail("schematron pattern didnt match")
          }
        }
      }
    )
  }

  // should get validation output file on a validation failure
  @Test def validationFailure(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/string.dfdl.xsd")
    val schematron = path("daffodil-schematron/src/test/resources/sch/always-fails.sch")
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/uuid.txt")

    withTempFile(
      ".conf",
      { conf =>
        withTempFile { svrl =>
          makeConf(conf, schematron, svrl)

          runCLI(args"parse --validate schematron=$conf -s $schema $input") { cli =>
            cli.expect("<never-fails>2f6481e6-542c-11eb-ae93-0242ac130002</never-fails>")
          }(ExitCode.ParseError)

          XML.loadFile(svrl.toFile) match {
            case Elem("svrl", "schematron-output", _, _, rules @ _*) =>
              val res = rules.find { r =>
                r match {
                  case Elem("svrl", "failed-assert", _, _, _*) => true
                  case _ => false
                }
              }
              // we should have found some failures
              assertTrue(res.isDefined)
            case _ =>
              fail("schematron pattern didnt match")
          }
        }
      }
    )
  }

  // shouldnt get a validation output file on parse failure
  // based on negative test test_996_CLI_Parsing_negativeTest04
  @Test def parseFailure(): Unit = {
    val schema = path(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd"
    )
    val schematron = path("daffodil-schematron/src/test/resources/sch/never-fails.sch")

    withTempFile(
      ".conf",
      { conf =>
        withTempFile { svrl =>
          makeConf(conf, schematron, svrl)

          runCLI(args"parse --validate schematron=$conf -s $schema -r unknown") { cli =>
            cli.send("12", inputDone = true)
            cli.expectErr("No root element found for unknown in any available namespace")
          }(ExitCode.UnableToCreateProcessor)
        }
      }
    )
  }

  // parse should fail with validation diagnostic when unable to write to the specified location
  @Test def outputPathFailure(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/string.dfdl.xsd")
    val schematron = path("daffodil-schematron/src/test/resources/sch/never-fails.sch")
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/uuid.txt")
    val svrl = path("thisisnotavalidlocation/schematron.svrl")

    withTempFile(
      ".conf",
      { conf =>
        makeConf(conf, schematron, svrl)

        runCLI(args"parse --validate schematron=$conf -s $schema $input") { cli =>
          cli.expectErr("[error] Validation Error")
        }(ExitCode.ParseError)
      }
    )
  }

  // validator output should overwrite existing file
  @Test def overwriteExistingFile(): Unit = {
    val schema = path("daffodil-schematron/src/test/resources/xsd/string.dfdl.xsd")
    val schematron = path("daffodil-schematron/src/test/resources/sch/never-fails.sch")
    val input = path("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/uuid.txt")

    withTempFile(
      ".conf",
      { conf =>
        withTempFile { svrl =>
          Files.write(svrl, "=== this content will be overwritten ===".getBytes(UTF_8), APPEND)

          makeConf(conf, schematron, svrl)

          runCLI(args"parse --validate schematron=$conf -s $schema $input") { cli =>
            cli.expect("<never-fails>2f6481e6-542c-11eb-ae93-0242ac130002</never-fails>")
          }(ExitCode.Success)

          XML.loadFile(svrl.toFile) match {
            case Elem("svrl", "schematron-output", _, _, rules @ _*) =>
              val res = rules.find { r =>
                r match {
                  case Elem("svrl", "failed-assert", _, _, _*) => true
                  case _ => false
                }
              }
              // we should not have found failures
              assertFalse(res.isDefined)
            case _ =>
              fail("schematron pattern didnt match")
          }
        }
      }
    )
  }

}
