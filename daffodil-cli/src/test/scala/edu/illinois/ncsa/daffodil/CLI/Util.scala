/* Copyright (c) 2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.CLI

import edu.illinois.ncsa.daffodil.util._
import net.sf.expectit.ExpectBuilder
import net.sf.expectit.Expect
import net.sf.expectit.filter.Filters.replaceInString
import java.nio.file.Paths
import scala.collection.JavaConverters._
import java.util.concurrent.TimeUnit
import edu.illinois.ncsa.daffodil.xml.XMLUtils

object Util {

  //val testDir = "daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/"
  val testDir = "/edu/illinois/ncsa/daffodil/CLI/"
  val outputDir = testDir + "output/"

  val isWindows = System.getProperty("os.name").toLowerCase().startsWith("windows")

  val dafRoot = sys.env.get("DAFFODIL_HOME").getOrElse(".")

  def daffodilPath(dafRelativePath: String): String = {
    XMLUtils.slashify(dafRoot) + dafRelativePath
  }

  val binPath = Paths.get(dafRoot, "daffodil-cli", "target", "universal", "stage", "bin", String.format("daffodil%s", (if (isWindows) ".bat" else ""))).toString()

  def getExpectedString(filename: String, convertToDos: Boolean = false): String = {
    val rsrc = Misc.getRequiredResource(outputDir + filename)
    //val source = scala.io.Source.fromFile(outputDir + filename)
    val source = scala.io.Source.fromFile(rsrc)
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
    str.replaceAll("/", "\\\\")
  }

  def fileConvert(str: String): String = {
    val newstr = str.replaceAll("\r\n", "\n")
    return newstr
  }
}
