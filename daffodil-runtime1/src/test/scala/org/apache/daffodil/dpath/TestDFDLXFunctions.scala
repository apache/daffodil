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

package org.apache.daffodil.dpath

import org.junit.Test
import sys.process._
import java.nio.file.Paths
import org.apache.daffodil.xml.XMLUtils

class TestDFDLXFunctions{

    val DAFFODIL_JAVA_OPTS = Map("DAFFODIL_JAVA_OPTS" -> "-Xms256m -Xmx2048m -Dfile.encoding=UTF-8")

      def cmdConvert(str: String): String = {
    if (isWindows)
      str.replaceAll("/", "\\\\")
    else
      str
  }

    val isWindows = System.getProperty("os.name").toLowerCase().startsWith("windows")

  val dafRoot = sys.env.get("DAFFODIL_HOME").getOrElse(".")

  def daffodilPath(dafRelativePath: String): String = {
    XMLUtils.slashify(dafRoot) + dafRelativePath
  }

   def devNull(): String = {
    if (isWindows) {
      "NUL"
    } else {
      "/dev/null"
    }
  }

  val binPath = Paths.get(dafRoot, "daffodil-cli", "target", "universal", "stage", "bin", String.format("daffodil%s", (if (isWindows) ".bat" else ""))).toString()

  @Test def test_2575_DFDLX_Trace_output(): Unit = {
    val schemaFile = daffodilPath("daffodil-runtime1/src/main/resources/test/validation/trace_output.dfdl.xsd")
    val inputFile = daffodilPath("daffodil-runtime1/src/main/resources/test/validation/trace_output.txt.xml")
    val (testSchemaFile, testInputFile) = if (isWindows) (cmdConvert(schemaFile), cmdConvert(inputFile)) else (schemaFile, inputFile)

    try{
      
      val cmd = String.format("%s unparse -r output -s %s -o %s %s", binPath, testSchemaFile, devNull, testInputFile)

      val stdout = new StringBuilder
      val stderr = new StringBuilder
      val status = cmd ! ProcessLogger(stdout append _, stderr append _)
      assert(stderr.toString.equals(""))
    } finally {
    }
  }

}