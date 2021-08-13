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

package org.apache.daffodil

import net.sf.expectit.MultiResult
import org.apache.daffodil.CLI.Util
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof
import net.sf.expectit.matcher.Matchers.regexp
import net.sf.expectit.matcher.Matchers.sequence
import net.sf.expectit.Result
import net.sf.expectit.matcher.Matcher
import org.apache.daffodil.Main.ExitCode

import java.io.File
import java.io.FileOutputStream
import java.nio.file.Path
import java.nio.file.Paths
import scala.util.matching.Regex

/**
 * Reference implementation for enhancements called for in DAFFODIL-2381, implemented specifically for Schematron.
 *
 * There are a number of bits in here that need abstracted to apply it across all CLI tests but it demonstrates one way
 * to roll up the repetative operations required in the CLI tests.
 *
 * One less obvious schematron specific bit in these tests is the path resolution in the command line using mustache
 * brackets.  There is a difference between where single and double brackets base the root of their paths from.  This
 * is documented below in more detail.
 *
 */
package object schematron {
  val FailureErrorCode = 1
  val JoinStdError = true

  def resolvePath(argstring: String): String =
    mustache.replaceAllIn(argstring, _ match {
      case mustache2(p) => schPath(p)
      case mustache1(p) => cliPath(p)
    })

  /**
   * executes a command in a shell with the provided expectations and error code using a mustache syntax looks up files
   * from local resources {path} or the daffodil-schematron resources {{path}}
   * @param ec expected error code
   * @param stderr join stderr in output
   * @param body 2 tuple of daffodil arguments and expectation
   */
  def withShell[R <: Result](ec: ExitCode.Value, stderr: Boolean = false)(body: => (String,  Matcher[R])): Unit = {
    val (argstring, expectation) = body
    val args = resolvePath(argstring)

    val joinStdErr = if(stderr) "2>&1" else ""
    val cmd = Util.binPath :: args :: joinStdErr :: Nil mkString " "
    val shell = Util.start("")
    try {
      shell.sendLine(cmd).expect(expectation)

      Util.expectExitCode(ec, shell)

      shell.sendLine("exit")
      shell.expect(eof)
      shell.close()
    } finally {
      shell.close()
    }
  }

  // two reasons for this bit of parsing indirection
  // 1. support consuming resources from multiple projects
  // 2. avoid doing the noisy and repetetive resolution in the unit tests
  private val mustache = """\{{1,2}(.+?)}{1,2}""".r.unanchored
  private val mustache1 = """\{(.+?)}""".r.unanchored
  private val mustache2 = """\{\{(.+?)}}""".r.unanchored

  // have to use platform specific matching here as the $ doesnt match on Windows
  // potentially because of the withInputFilters set on the shell in Util


  private def schPath(p: String): String = fixpath(s"daffodil-schematron/src/test/resources/$p")
  private def cliPath(p: String): String = fixpath(s"daffodil-cli/src/it/resources/org/apache/daffodil/CLI/$p")
  private def fixpath(p: String): String = {
    val full = Paths.get(Util.dafRoot, p).toString
    val argfix = full.replaceAll("""\\""", "/")
    Regex.quoteReplacement(argfix)
  }

  /**
   * number of lines, any content on those lines
   * @param n line count
   * @return
   */
  def anyLines(n: Int): Matcher[_] = regexp(Seq.fill(n)(s".+$eol").mkString)

  /**
   * make a temp file containing the bytes
   * @param str
   * @return
   */
  def mktmp(d: Array[Byte], prefix: String = "schval", suffix: String = "data"): Path = {
    val f = File.createTempFile(prefix, suffix)
    f.deleteOnExit()
    val os = new FileOutputStream(f)
    os.write(d)
    os.close()
    f.toPath
  }
  def mktmp(str: String): Path = mktmp(str.getBytes)

  def mkTmpConf(schPath: String, svrlPath: Path): String = {
    val svrl = Regex.quoteReplacement(svrlPath.toString.replaceAll("""\\""", "/"))
    val sch = resolvePath(s"{{$schPath}}")
    mktmp(
      s"""schematron.path="$sch"
         |schematron.svrl.file="$svrl"
         |""".stripMargin.getBytes, suffix = ".conf").toString
  }

  /**
   * the common pattern on stderr when a validation error is hit
   * @param txt optional additional validation error text
   * @return matcher
   */
  def validationError(txt: String = ""): Matcher[MultiResult] =
    sequence(contains(s"[error] Validation Error: $txt"), anyLines(3))

  private lazy val eol = "\n"
  def lineEndsWith(txt: String): Matcher[Result] = contains(s"$txt$eol")
  def lineEndsWithRegex(pattern: String): Matcher[Result] = regexp(s"$pattern$eol")

  def regexLine(pattern: String): Matcher[Result] = regexp(s"$pattern$eol")
}
