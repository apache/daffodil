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

package org.apache.daffodil.blob

import java.net.URI
import java.nio.file.Files.exists
import java.nio.file.Path
import java.nio.file.Paths

import org.apache.commons.io.FileUtils

import org.junit.Test
import org.junit.Assume.assumeTrue
import org.junit.Assert.assertEquals

import scala.io.Source

import org.apache.daffodil.CLI.Util._
import org.apache.daffodil.Main.ExitCode

class TestBlob {

  /***
   * ---- Blob Generation Instructions ----
   *
   * These large file tests are commented out so that they are not triggered on
   * automatic regression tests on the build servers.  In order to run them you
   * will need to generate the test file(s) using the gen_blob.py script. It can
   * be found in and should be run from the directory:
   *
   * daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/
   *
   * Please note that the md5sum that is printed out from the script is the hash
   * of just the blob portion of the file that is generated, it does not include
   * the first 8 bytes of the file, which is the length of the blob.
   *
   * Make sure that the generated file is located in the following directory:
   *
   * daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/
   *
   * The exact command used to generate the blob for each test will be listed in
   * a comment above that test
   ***/

  private def findInfosetBlob(path: Path): Path = {
    val contents = Source.fromFile(path.toFile).mkString
    val blob = contents.substring(contents.indexOf("file://")).takeWhile(_ != '<')
    Paths.get(new URI(blob))
  }

  /**
   * The CLI puts blobs in user.dir / "daffodil-blobs", which cannot be
   * changed. This should wrap CLI runs so that the blob dir is deleted at the
   * end
   */
  private def withBlobDir(f: => Unit): Unit = {
    try {
      f
    } finally {
      val blobDir = Paths.get(System.getProperty("user.dir"), "daffodil-blobs")
      FileUtils.deleteDirectory(blobDir.toFile)
    }
  }

  /***
   * Command to generate blob file:
   *
   * python gen_blob.py -s 1 -o 1MB.bin
   *
   ***/
  @Test def test_1MB_blob(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/large_blob.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/1MB.bin")

    assumeTrue("large test input file must be manually generated", exists(input))

    withTempFile { infoset =>
      withTempFile { unparse =>
        withBlobDir {
          runCLI(args"parse -s $schema -o $infoset $input") { cli =>
          } (ExitCode.Success)

          runCLI(args"unparse -s $schema -o $unparse $infoset") { cli =>
          } (ExitCode.Success)

          val blob = findInfosetBlob(infoset)
          assertEquals("bc8f9d01382bf12248747cd6faecbc59", md5sum(blob))
          assertEquals("72d1f935d7fff766d011757ae03d5b1d", md5sum(unparse))
        }
      }
    }
  }

  /***
   * Command to generate blob file:
   *
   * python gen_blob.py -s 2049 -o 2049MB.bin
   *
   ***/
  @Test def test_2GB_blob(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/large_blob.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/2049MB.bin")

    assumeTrue("large test input file must be manually generated", exists(input))

    withTempFile { infoset =>
      withTempFile { unparse =>
        withBlobDir {
          runCLI(args"parse -s $schema -o $infoset $input", timeout = 120) { cli =>
          } (ExitCode.Success)

          runCLI(args"unparse -s $schema -o $unparse $infoset", timeout = 120) { cli =>
          } (ExitCode.Success)

          val blob = findInfosetBlob(infoset)
          assertEquals("c5675d3317725595d128af56a624c49f", md5sum(blob))
          assertEquals("2435c33e55aae043fc9b28f38f5cc2e9", md5sum(unparse))
        }
      }
    }
  }

  /***
   * Please note that this uses the same file as test_2GB_blob.
   *
   * Command to generate blob file:
   *
   * python gen_blob.py -s 2049 -o 2049MB.bin
   *
   ***/
  @Test def test_blob_backtracking(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/blob_backtracking.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/2049MB.bin")

    assumeTrue("large test input file must be manually generated", exists(input))

    withBlobDir {
      runCLI(args"parse -s $schema $input", timeout = 120) { cli =>
        cli.expectErr("Attempted to backtrack too far")
      } (ExitCode.ParseError)
    }
  }

  /***
   * Please note that this uses the same file as test_2GB_blob.
   *
   * Command to generate blob file:
   *
   * python gen_blob.py -s 2049 -o 2049MB.bin
   *
   ***/
  @Test def test_blob_backtracking_streaming_fail(): Unit = {
    val schema = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/blob_backtracking.dfdl.xsd")
    val input = path("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/2049MB.bin")

    assumeTrue("large test input file must be manually generated", exists(input))

    withBlobDir {
      runCLI(args"parse -s $schema", timeout = 120) { cli =>
        cli.sendFile(input, inputDone = true)
        cli.expectErr("Attempted to backtrack too far")
      } (ExitCode.ParseError)
    }
  }

}
