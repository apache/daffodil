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

import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.io.PipedInputStream
import java.io.PipedOutputStream
import java.io.PrintStream
import java.lang.ProcessBuilder
import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.security.MessageDigest
import java.util.concurrent.TimeUnit

import scala.collection.JavaConverters._
import scala.collection.mutable

import com.fasterxml.jackson.core.io.JsonStringEncoder

import net.sf.expectit.Expect
import net.sf.expectit.ExpectBuilder
import net.sf.expectit.Result
import net.sf.expectit.filter.Filters.replaceInString
import net.sf.expectit.matcher.Matcher
import net.sf.expectit.matcher.Matchers.contains

import org.apache.commons.io.FileUtils

import org.apache.logging.log4j.Level
import org.apache.logging.log4j.core.appender.OutputStreamAppender
import org.apache.logging.log4j.core.config.AbstractConfiguration
import org.apache.logging.log4j.core.config.ConfigurationSource
import org.apache.logging.log4j.core.config.Configurator
import org.apache.logging.log4j.core.layout.PatternLayout

import org.junit.Assert.assertEquals

import org.apache.daffodil.Main
import org.apache.daffodil.Main.ExitCode

object Util {

  private val isWindows = System.getProperty("os.name").toLowerCase().startsWith("windows")

  private val daffodilRoot = sys.env.getOrElse("DAFFODIL_HOME", ".")

  private val daffodilBinPath = {
    val ext = if (isWindows) ".bat" else ""
    Paths.get(daffodilRoot, s"daffodil-cli/target/universal/stage/bin/daffodil$ext")
  }

  /**
   * Convert the daffodilRoot + parameter to a java Path. The string
   * parameter should contain unix path sparators and it will be interpreted
   * correctly regardless of operating system. When converted to a string to
   * send to the CLI, it will use the correct line separator for the
   * operating system
   */
  def path(string: String): Path = {
    Paths.get(daffodilRoot, string)
  }

  def devNull(): String = if (isWindows) "NUL" else "/dev/null"

  def md5sum(path: Path): String = {
    val md = MessageDigest.getInstance("MD5")
    val buffer = new Array[Byte](8192)
    val stream = Files.newInputStream(path)
    var read = 0
    while ({read = stream.read(buffer); read} > 0) {
      md.update(buffer, 0, read)
    }
    val md5sum = md.digest()
    val bigInt = new BigInteger(1, md5sum)
    bigInt.toString(16)
  }

  /**
   * Create a temporary file in /tmp/daffodil/, call a user provided function
   * passing in the Path to that new file, and delete the file when the
   * function returns.
   */
  def withTempFile(f: (Path) => Unit) : Unit = withTempFile(null, f)

  /**
   * Create a temporary file in /tmp/daffodil/ with a givin suffix, call a user
   * provided function passing in the Path to that new file, and delete the
   * file when the function returns.
   */
  def withTempFile(suffix: String, f: (Path) => Unit): Unit = {
    val tempRoot = Paths.get(System.getProperty("java.io.tmpdir"), "daffodil")
    Files.createDirectories(tempRoot)
    val tempFile = Files.createTempFile(tempRoot, "daffodil-", suffix)
    try {
      f(tempFile)
    } finally {
      tempFile.toFile.delete()
    }
  }

  /**
   * Create a temporary directory in /tmp/daffodil/, call a user provided
   * function passing in the Path to that new directory, and delete the
   * directory and all of its contents when the function returns
   */
  def withTempDir(f: (Path) => Unit): Unit = {
    val tempRoot = Paths.get(System.getProperty("java.io.tmpdir"), "daffodil")
    Files.createDirectories(tempRoot)
    val tempDir = Files.createTempDirectory(tempRoot, "daffodil-")
    try {
      f(tempDir)
    } finally {
      FileUtils.deleteDirectory(tempDir.toFile)
    }
  }

  /**
   * Set a system property using a provided key, value tuple, call a user
   * provided function, and reset or clear the property when the function
   * returns.
   */
  def withSysProp(keyVal: (String, String))(f: => Unit): Unit = {
    val key = keyVal._1
    val newVal = keyVal._2
    val oldVal = System.setProperty(key, newVal)
    try {
      f
    } finally {
      if (oldVal == null) {
        System.clearProperty(key)
      } else {
        System.setProperty(key, oldVal)
      }
    }
  }

  /**
   * Run a CLI test.
   *
   * Runs CLI logic using the provided arguments and classpath, creates a
   * CLITester so that the user can send input and validate output, and
   * verifies the expected exit code.
   *
   * For performance reasons, this defaults to running the CLI in a new thread
   * unless the classpaths parameter is nonempty or the fork parameter is set to
   * true. Otherwise a new process is spawned.
   *
   * @param args arguments to pass to the CLI. This should not include the
   *   daffodil binary
   * @param classpaths sequence of paths to add to the classpath. If non-empty,
   *   runs the CLI in a new process instead of a thread and will likely decrease
   *   performance
   * @param fork if true, forces the the CLI to run in a new process
   * @param timeout how long to wait, in seconds, for the CLI to exit after the
   *   testFunc has returned. Also how long to wait for individual expect
   *   operations in the CLITester
   * @param debug if true, prints arguments and classpath information to
   *   stdout. Also echos all CLITester input and output to stdout.
   * @param testFunc function to call to send input to the CLI and validate
   *   output from CLI stdout/stderr
   * @param expectedExitCode the expected exit code of the CLI
   *
   * @throws AssertionError if the actual exit code does not match the expected exit code
   * @throws ExpectIOException if a CLITester expect validation operation fails
   */
  def runCLI
    (args: Array[String], classpaths: Seq[Path] = Seq(), fork: Boolean = false, timeout: Int = 10, debug: Boolean = false)
    (testFunc: (CLITester) => Unit)
    (expectedExitCode: ExitCode.Value): Unit = {

    val (toIn, fromOut, fromErr, threadOrProc: Either[CLIThread, Process]) =
      if (classpaths.nonEmpty || fork) {
        // spawn a new process to run Daffodil, needed if a custom classpath is
        // defined or if the caller explicitly wants to fork
        val processBuilder = new ProcessBuilder()

        if (classpaths.nonEmpty) {
          val classpath = classpaths.mkString(File.pathSeparator)
          if (debug) System.out.println(s"DAFFODIL_CLASSPATH=$classpath")
          processBuilder.environment().put("DAFFODIL_CLASSPATH", classpath)
        }

        val cmd = daffodilBinPath.toString +: args
        if (debug) System.out.println(cmd.mkString(" "))
        processBuilder.command(cmd.toList.asJava)

        val process = processBuilder.start()

        val toIn = process.getOutputStream()
        val fromOut = process.getInputStream()
        val fromErr = process.getErrorStream()
        (toIn, fromOut, fromErr, Right(process))
      } else {
        // create a new thread for the CLI test to run, using piped
        // input/output streams to connect the thread and the CLITester
        val in = new PipedInputStream()
        val toIn = new PipedOutputStream(in)

        val out = new PipedOutputStream()
        val fromOut = new PipedInputStream(out)

        val err = new PipedOutputStream()
        val fromErr = new PipedInputStream(err)

        if (debug) System.out.println("daffodil " + args.mkString(" "))

        val thread = new CLIThread(args, in, out, err)
        thread.start()
        (toIn, fromOut, fromErr, Left(thread))
      }

    val eb = new ExpectBuilder()
    eb.withOutput(toIn)
    eb.withInputs(fromOut, fromErr)
    eb.withInputFilters(replaceInString("\r\n", "\n"))
    eb.withTimeout(timeout, TimeUnit.SECONDS)
    eb.withExceptionOnFailure()
    if (debug) {
      eb.withEchoOutput(System.out)
      eb.withEchoInput(System.out)
    }
    val expect = eb.build()
    val tester = new CLITester(expect, toIn)

    try {
      testFunc(tester)
    } finally {
      threadOrProc match {
        case Left(thread) => thread.join(timeout * 1000)
        case Right(process) => process.waitFor(timeout, TimeUnit.SECONDS)
      }
      expect.close()
      toIn.close()
      fromOut.close()
      fromErr.close()
    }

    val actualExitCode = threadOrProc match {
      case Left(thread) => thread.exitCode
      case Right(process) => ExitCode(process.exitValue)
    }
    assertEquals("Incorrect exit code,", expectedExitCode, actualExitCode)
  }

  /**
   * A class to run the CLI in a thread instead of a new process, given the
   * arguments to use (excluding the daffodil binary) and streams to use for
   * stdin/out/err.
   */
  private class CLIThread(args: Array[String], in: InputStream, out: OutputStream, err: OutputStream) extends Thread {
    var exitCode = ExitCode.Failure

    override def run(): Unit = {
      val psOut = new PrintStream(out)
      val psErr = new PrintStream(err)

      // configure the CLI and log4j to use our custom streams, nothing should
      // actually use stdin/stdout/stderr
      Main.setInputOutput(in, psOut, psErr)
      configureLog4j(psErr)

      exitCode = Main.run(args)
    }

    /**
     * By default log4j outputs to stderr. This changes that so it writes to a
     * provided PrintStream which is connected to the CLITester, allowing tests
     * to expect content written by log4j. This also defines the same pattern
     * used by the CLI from the daffodil-cli/src/conf/log4j2.xml config
     * file--we duplicate it here because the normal log4j config file targets
     * stderr while we need to target the provided PrintStream (which can only
     * be defined programatically). It was also found to be too difficult to
     * load the config file and mutate the log4j configuration to write to this
     * PrintStream instead of stderr.
     */
    private def configureLog4j(ps: PrintStream): Unit = {
      val config = new AbstractConfiguration(null, ConfigurationSource.NULL_SOURCE) {
        override def doConfigure(): Unit = {
          val appenderName = "DaffodilCli"

          val layout = PatternLayout.newBuilder()
            .withPattern("[%p{lowerCase=true}] %m%n")
            .withConfiguration(this)
            .build()

          val appenderBuilder: OutputStreamAppender.Builder[_] = OutputStreamAppender.newBuilder()
          appenderBuilder.setName(appenderName)
          appenderBuilder.setLayout(layout)
          appenderBuilder.setTarget(ps)
          appenderBuilder.setConfiguration(this)
          val appender = appenderBuilder.build()

          val rootLogger = getRootLogger()
          rootLogger.setLevel(Level.WARN);
          rootLogger.addAppender(appender, null, null)
        }
      }
      Configurator.reconfigure(config)
    }
  }

  /**
   * Wrapper around Expect to make integration tests less verbose. It also
   * supports closing the mimicked stdin input stream (via the closeInput()
   * function or the inputDone parameter to the send*() functions), which is
   * sometimes needed since Daffodil may need to receive an EOF before it can
   * finish parsing.
   */
  private class CLITester(expect: Expect, toIn: OutputStream) {

    /**
     * Close stdin, triggering an EOF.
     */
    def closeInput(): Unit = { toIn.close() }

    /**
     * Write a string to stdin. This does not incluede trailing newline. If
     * inputDone is true, close stdin afterwards.
     */
    def send(string: String, inputDone: Boolean = false): Unit = {
      expect.send(string)
      if (inputDone) closeInput()
    }

    /**
     * Write a string to stdin with a trailing newline. If inputDone is true,
     * close stdin afterwards.
     */
    def sendLine(string: String, inputDone: Boolean = false): Unit = {
      expect.sendLine(string)
      if (inputDone) closeInput()
    }

    /**
     * Write an entire byte array to stdin. If inputDone is true, close stdin
     * afterwards.
     */
    def sendBytes(bytes: Array[Byte], inputDone: Boolean = false): Unit = {
      expect.sendBytes(bytes)
      if (inputDone) closeInput()
    }

    /**
     * Write a file to stdin. If inputDone is true, close stdin afterwards.
     */
    def sendFile(path: Path, inputDone: Boolean = false): Unit = {
      val chunkSize = 8192
      val buffer = new Array[Byte](chunkSize)
      val stream = Files.newInputStream(path)
      var read = 0
      while ({read = stream.read(buffer); read} > 0) {
        if (read == chunkSize) {
          expect.sendBytes(buffer)
        } else {
          // The expect.sendBytes function does not have parameters to send a
          // subset of an array, it just sends the whole array. So we need to
          // trim it down to the actual read size and send that
          val smaller = new Array[Byte](read)
          buffer.copyToArray(smaller, 0, read)
          expect.sendBytes(smaller)
        }
      }
      if (inputDone) closeInput()
    }

    def expect(matcher: Matcher[_]): Result = expect.expect(matcher)
    def expect(string: String): Result = expect.expect(contains(string))

    def expectErr(matcher: Matcher[_]): Result = expect.expectIn(1, matcher)
    def expectErr(string: String): Result = expect.expectIn(1, contains(string))
  }

  /**
   * Escapes a string that is expected to be a json string
   */
  def jsonEscape(string: String): String = {
    val chars = JsonStringEncoder.getInstance().quoteAsString(string)
    new String(chars)
  }

  /**
   * This "args" string interpoloator makes it easy to create an Array[String]
   * used for CLI arguments. Only spaces in the "format" string are split on.
   * Spaces in an expressions in the format string are not split. For example
   *
   *   args"parse -s $schema $input".split(" ")
   *
   * Becomes someething like this:
   *
   *   Array("parse", "-s", "path/to/schema.dfdl.xsd", "path/to/input.bin")
   *
   * An alternative approach one might choose by using existing interpolators
   * is something like this:
   *
   *   s"parse -s $schema $input".split(" ")
   *
   * This issue with this approach is that if the $schema or $input variables
   * evaluate to something with string (which is not uncommon on some windows
   * systems), then we end up splitting those files paths into separate
   * arguments. This args interpolator ensures we don't split spaces that come
   * from expressions.
   *
   * Note that quotes cannot be used to prevent splitting. For example, this
   *
   *   args"quotes do 'not prevent' splitting"
   *
   * Results in the following:
   *
   *   Array("quotes", "do", "'not", "prevent'", "splitting")
   *
   * To prevent splitting on a particular space, then expressions can be used,
   * for example:
   *
   *   args"this ${"is split"} correctly"
   *
   * Which results in the following:
   *
   *   Array("this", "is split", "correctly")
   *
   * Note that this also handles concatenating expression correctly, for
   * example:
   *
   *   args"some --arg=$root/$value"
   *
   * Results in
   *
   *   Array("some", "--arg=the/result/of/root/and/value")
   *
   */
  implicit class ArgsHelper(val sc: StringContext) extends AnyVal {
    def args(exprs: Any*): Array[String] = {
      val strings = sc.parts.iterator
      val expressions = exprs.iterator
      val buf = mutable.ArrayBuffer[String]()

      // regex to split on spaces, but using positive lookahead and lookbehind
      // so the spaces end up in the array instead of being discarded. For
      // example, using this regular expression to split this string:
      //
      //   "parse --schema foo.xsd input"
      //
      // Results in the following:
      //
      //   Array("parse", " ", "--schema", " ", "foo.xsd", " ", "input")
      //
      // This is necessary so that after we build the buf array, the spaces
      // provide extra information about how to concatenate exprs and
      // StringContexts
      val regex = "((?= )|(?<= ))".r

      buf.appendAll(regex.split(strings.next()))
      while (strings.hasNext) {
        buf.append(expressions.next().toString)
        buf.appendAll(regex.split(strings.next()))
      }

      // At this point, we have an array that is mixed with arguments and
      // single spaces, for example, this string
      //
      //   args"parse --schema=$bar/$baz input"
      //
      // Now looks like this:
      //
      //   Array("parse", " ", "--schema=", "bar/result", "/", "baz/result", " ", "input")
      //
      // We want to concatent any array elements that aren't separated by a
      // space element and drop all the spaces. The following does this by
      // accumulating args until we hit a space and adds that to the
      // final array
      val (res, lastAccumulated) = buf.foldLeft((mutable.ArrayBuffer[String](), "")) { case ((resArray, accumulated), curVal) =>
        (accumulated, curVal) match {
          case ("", " ") => {
            // curVal is a space, but we have accumulated nothing. This means
            // there were multiple spaces in a row. Just ignore this curVal
            (resArray, "")
          }
          case (_, " ") => {
            // curVal is a space and we have accumulated an argument. Append
            // what we have accumulated to the array and reset the accumulated
            // string--we will start accumulating for the next arg
            (resArray :+ accumulated, "")
          }
          case _ => {
            // curVal is not a space. Do not modify the array, but concatenate
            // the curVal to what we've accumulated so far
            (resArray, accumulated + curVal)
          }
        }
      }
      // if non-empty, the last thing we accumulated must be appended to the array
      val args =
        if (lastAccumulated != "") {
          res :+ lastAccumulated
        } else {
          res
        }
      args.toArray
    }
  }
}
