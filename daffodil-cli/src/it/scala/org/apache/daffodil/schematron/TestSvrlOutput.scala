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

package org.apache.daffodil.schematron

import net.sf.expectit.matcher.Matchers.sequence
import org.apache.daffodil.CLI.Util
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

import java.nio.file.Path
import java.nio.file.Paths
import scala.xml.XML

class TestSvrlOutput {
  import TestValidating._

  @Test def validationSuccess(): Unit = {
    val svrlPath = makeTempFilePath()
    val confFile = mkTmpConf(never, svrlPath)
    withShell(stderr=true) {
      s"parse --validate schematron=$confFile -s {{$uuid}} {$data}" -> alwaysResult
    }

    val svrlFile = svrlPath.toFile
    assertTrue(svrlFile.exists())

    try {
      XML.loadFile(svrlFile) match {
        case <svrl:schematron-output>{rules @ _*}</svrl:schematron-output> =>
          val res = rules.find {
            case <svrl:failed-assert>{  _* }</svrl:failed-assert> => true
            case _ => false
          }
          // we should not have found failures
          assertFalse(res.isDefined)
        case _ =>
          fail("schematron pattern didnt match")
      }
    } finally {
      svrlFile.delete()
    }
  }

  // should get validation output file on a validation failure
  @Test def validationFailure(): Unit = {
    val svrlPath = makeTempFilePath()
    val confFile = mkTmpConf(always, svrlPath)
    withShell(FailureErrorCode) {
      s"parse --validate schematron=$confFile -s {{$uuid}} {$data}" -> alwaysResult
    }

    val svrlFile = svrlPath.toFile
    assertTrue(svrlFile.exists())

    try {
      XML.loadFile(svrlFile) match {
        case <svrl:schematron-output>{rules @ _*}</svrl:schematron-output> =>
          val res = rules.find {
            case <svrl:failed-assert>{  _* }</svrl:failed-assert> => true
            case _ => false
          }
          // we should have found some failures
          assertTrue(res.isDefined)
        case _ =>
          fail("schematron pattern didnt match")
      }
    } finally {
      svrlFile.delete()
    }
  }

  // shouldnt get a validation output file on parse failure
  // based on negative test test_996_CLI_Parsing_negativeTest04
  @Test def parseFailure(): Unit = {
    val svrlPath = makeTempFilePath()
    val confFile = mkTmpConf(never, svrlPath)
    val data = mktmp("12")
    withShell(FailureErrorCode, stderr = true) {
      val schemaFile = Util.daffodilPath(
        "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
      val schema = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
      s"parse --validate schematron=$confFile -s $schema -r unknown $data" ->
        lineEndsWith("No root element found for unknown in any available namespace")
    }

    val svrlFile = svrlPath.toFile
    if (svrlFile.exists()) {
      svrlFile.delete()
      fail("svrl file should not exist on failed parse")
    }
  }

  // parse should fail with validation diagnostic when unable to write to the specified location
  @Test def outputPathFailure(): Unit = {
    val badSvrlPath = Paths.get("thisisnotavalidlocation/schematron.svrl")
    val confFile = mkTmpConf(never, badSvrlPath)
    withShell(FailureErrorCode, JoinStdError) {
      s"""parse --validate schematron="$confFile" -s {{$uuid}} {$data}""" -> sequence(
        lineEndsWithRegex(s"\\[error] Validation Error: .+"),
        anyLines(2))
    }

    val svrlFile = badSvrlPath.toFile
    assertFalse(svrlFile.exists())
  }

  // validator output should overwrite existing file
  @Test def overwriteExistingFile(): Unit = {
    val svrlPath = mktmp("=== this content will be overwritten ===")
    val confFile = mkTmpConf(never, svrlPath)
    withShell() {
      s"parse --validate schematron=$confFile -s {{$uuid}} {$data}" -> alwaysResult
    }

    val svrlFile = svrlPath.toFile
    assertTrue(svrlFile.exists())

    try {
      XML.loadFile(svrlFile) match {
        case <svrl:schematron-output>{rules @ _*}</svrl:schematron-output> =>
          val res = rules.find {
            case <svrl:failed-assert>{  _* }</svrl:failed-assert> => true
            case _ => false
          }
          // we should not have found failures
          assertFalse(res.isDefined)
        case _ =>
          fail("schematron pattern didnt match")
      }
    } finally {
      svrlFile.delete()
    }
  }

  private def makeTempFilePath(): Path = Paths.get(System.getProperty("java.io.tmpdir"), "schTestRaw.svrl")
}
