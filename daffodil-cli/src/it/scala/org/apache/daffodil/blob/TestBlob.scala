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

import junit.framework.Assert._
import org.junit.Test
import java.io.File
import org.apache.daffodil.CLI.Util
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof
import net.sf.expectit.Expect

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


  /***
   * Command to generate blob file:
   *
   * python gen_blob.py -s 1 -o 1MB.bin
   *
   ***/
  /*@Test*/ def test_1MB_blob() {

    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/large_blob.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/1MB.bin")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.start("")

    try {
      val cmd = String.format("%s parse -s %s %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      val result = shell.expect(contains("largeBlob")).toString
      // Use +7 to drop the 'file://' to get the path of the file
      val generated_blob = result.substring(result.indexOf("file://") + 7).takeWhile(_ != '<')

      shell.sendLine(Util.md5sum(generated_blob))
      shell.expect(contains("bc8f9d01382bf12248747cd6faecbc59"))

      // Clean up blobs
      if (Util.isWindows)
        shell.sendLine("rmdir /Q /S daffodil-blobs")
      else
        shell.sendLine("rm -rf daffodil-blobs")

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  /***
   * Command to generate blob file:
   *
   * python gen_blob.py -s 2049 -o 2049MB.bin
   *
   ***/
  /*@Test*/ def test_2GB_blob() {

    val DAFFODIL_JAVA_OPTS = Map("DAFFODIL_JAVA_OPTS" -> "-Xms256m -Xmx512m")
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/large_blob.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/2049MB.bin")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("", envp = DAFFODIL_JAVA_OPTS)

    try {
      // Execute Daffodil
      val cmd = String.format("%s parse -s %s %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      val result = shell.expect(contains("largeBlob")).toString
      // Use +7 to drop the 'file://' to get the path of the file
      val generated_blob = result.substring(result.indexOf("file://") + 7).takeWhile(_ != '<')

      // Compare blobs
      shell.sendLine(Util.md5sum(generated_blob))
      shell.expect(contains("c5675d3317725595d128af56a624c49f"))

      // Clean up blobs
      shell.sendLine(Util.rmdir("daffodil-blobs"))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
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
  /*@Test*/ def test_blob_backtracking() {

    val DAFFODIL_JAVA_OPTS = Map("DAFFODIL_JAVA_OPTS" -> "-Xms256m -Xmx512m")
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/blob_backtracking.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/2049MB.bin")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("", envp = DAFFODIL_JAVA_OPTS)

    try {
      // Execute Daffodil
      val cmd = String.format("%s parse -s %s %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expectIn(1, contains("Attempted to backtrack too far"))

      // Clean up blobs
      shell.sendLine(Util.rmdir("daffodil-blobs"))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
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
  /*@Test*/ def test_blob_backtracking_streaming_fail() {

    val DAFFODIL_JAVA_OPTS = Map("DAFFODIL_JAVA_OPTS" -> "-Xms256m -Xmx512m")
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/blob_backtracking.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/2049MB.bin")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = Util.startIncludeErrors("", envp = DAFFODIL_JAVA_OPTS)

    try {
      // Execute Daffodil
      val cmd = String.format(Util.cat(testInputFile) + " | %s parse --stream -s %s", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expectIn(1, contains("Attempted to backtrack too far"))

      // Clean up blobs
      shell.sendLine(Util.rmdir("daffodil-blobs"))

      shell.send("exit\n")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

}
