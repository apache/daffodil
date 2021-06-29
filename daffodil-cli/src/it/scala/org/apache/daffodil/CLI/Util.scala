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

package org.apache.daffodil.CLI

import org.apache.daffodil.util.Misc
import net.sf.expectit.ExpectBuilder
import net.sf.expectit.Expect
import net.sf.expectit.filter.Filters.replaceInString
import java.nio.file.Paths
import java.io.{File, PrintWriter}
import scala.collection.JavaConverters._
import java.util.concurrent.TimeUnit

import org.apache.daffodil.xml.XMLUtils

object Util {

  //val testDir = "daffodil-cli/src/it/resources/org/apache/daffodil/CLI/"
  val testDir = "/org/apache/daffodil/CLI/"
  val outputDir = testDir + "output/"

  val isWindows = System.getProperty("os.name").toLowerCase().startsWith("windows")

  val dafRoot = sys.env.get("DAFFODIL_HOME").getOrElse(".")

  def daffodilPath(dafRelativePath: String): String = {
    XMLUtils.slashify(dafRoot) + dafRelativePath
  }

  val binPath = Paths.get(dafRoot, "daffodil-cli", "target", "universal", "stage", "bin", String.format("daffodil%s", (if (isWindows) ".bat" else ""))).toString()

  def getExpectedString(filename: String, convertToDos: Boolean = false): String = {
    val rsrc = Misc.getRequiredResource(outputDir + filename)
    val is = rsrc.toURL.openStream()
    val source = scala.io.Source.fromInputStream(is)
    val lines = source.mkString.trim()
    source.close()
    fileConvert(lines)
  }

  def start(cmd: String, expectErr: Boolean = false, envp: Map[String, String] = Map.empty[String, String], timeout: Long = 30): Expect = {
    val spawnCmd = if (isWindows) {
      "cmd /k " + cmdConvert(cmd)
    } else {
      "/bin/bash"
    }

    return getShell(cmd, spawnCmd, expectErr, envp, timeout)
  }

  def startIncludeErrors(cmd: String, envp: Map[String, String] = Map.empty[String, String], timeout: Long = 30): Expect = {
    val spawnCmd = if (isWindows) {
      "cmd /k" + cmdConvert(cmd)
    } else {
      "/bin/bash"
    }

    getShellWithErrors(cmd, spawnCmd, envp, timeout)
  }

  // This function will be used if you are providing two separate commands
  // and doing the os check on the 'front end' (not within this utility class)
  def startNoConvert(cmd: String, envp: Map[String, String] = Map.empty[String, String], timeout: Long = 30): Expect = {
    val spawnCmd = if (isWindows) {
      "cmd /k" + cmd
    } else {
      "/bin/bash"
    }

    return getShell(cmd, spawnCmd, envp = envp, timeout = timeout)
  }

  def getShell(cmd: String, spawnCmd: String, expectErr: Boolean = false, envp: Map[String, String] = Map.empty[String, String], timeout: Long): Expect = {
    val newEnv = System.getenv().asScala ++ envp

    val envAsArray = newEnv.toArray.map { case (k, v) => k + "=" + v }
    val process = Runtime.getRuntime().exec(spawnCmd, envAsArray)
    val inputStream = if (expectErr) {
      process.getErrorStream()
    } else {
      process.getInputStream()
    }
    val shell = new ExpectBuilder()
      .withInputs(inputStream)
      .withInputFilters(replaceInString("\r\n", "\n"))
      .withOutput(process.getOutputStream())
      .withEchoOutput(System.out)
      .withEchoInput(System.out)
      .withTimeout(timeout, TimeUnit.SECONDS)
      .withExceptionOnFailure()
      .build();
    if (!isWindows) {
      shell.send(cmd)
    }
    return shell
  }

  // Return a shell object with two streams
  // The inputStream will be at index 0
  // The errorStream will be at index 1
  def getShellWithErrors(cmd: String, spawnCmd: String, envp: Map[String, String] = Map.empty[String, String], timeout: Long): Expect = {
    val newEnv = System.getenv().asScala ++ envp

    val envAsArray = newEnv.toArray.map { case (k, v) => k + "=" + v }
    val process = Runtime.getRuntime().exec(spawnCmd, envAsArray)
    val shell = new ExpectBuilder()
      .withInputs(process.getInputStream(), process.getErrorStream())
      .withInputFilters(replaceInString("\r\n", "\n"))
      .withOutput(process.getOutputStream())
      .withEchoOutput(System.out)
      .withEchoInput(System.out)
      .withTimeout(timeout, TimeUnit.SECONDS)
      .withExceptionOnFailure()
      .build();
    if (!isWindows) {
      shell.send(cmd)
    }
    return shell
  }

  def cmdConvert(str: String): String = {
    if (isWindows)
      str.replaceAll("/", "\\\\")
    else
      str
  }

  def fileConvert(str: String): String = {
    val newstr = str.replaceAll("\r\n", "\n")
    return newstr
  }

  def echoN(str: String): String = {
    if (isWindows) {
      "echo|set /p=" + str
    } else {
      "echo -n " + str
    }
  }

  def devNull(): String = {
    if (isWindows) {
      "NUL"
    } else {
      "/dev/null"
    }
  }

  def makeMultipleCmds(cmds: Array[String]): String = {
    if (isWindows) {
      cmds.mkString(" & ")
    } else {
      cmds.mkString("; ")
    }
  }

  def md5sum(blob_path: String): String = {
    if (isWindows) {
      String.format("certutil -hashfile %s MD5", blob_path)
    } else {
      String.format("md5sum %s", blob_path)
    }
  }

  def rmdir(path: String): String = {
    if (Util.isWindows)
      String.format("rmdir /Q /S %s", path)
    else
      String.format("rm -rf %s", path)
  }

  def cat(str: String): String = {
    if (isWindows) {
      "type " + str
    } else {
      "cat " + str
    }
  }

  def newTempFile(filePrefix: String, fileSuffix: String, optFileContents: Option[String] = None): File = {
    val inputFile = File.createTempFile(filePrefix, fileSuffix)
    inputFile.deleteOnExit
    if (optFileContents.nonEmpty) {
      val contents = optFileContents.get
      val pw = new PrintWriter(inputFile)
      pw.write(contents)
      pw.close
    }
    inputFile
  }
}
