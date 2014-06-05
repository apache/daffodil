package edu.illinois.ncsa.daffodil.CLI

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

import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.util._
import net.sf.expectit.ExpectBuilder
import net.sf.expectit.Expect
import net.sf.expectit.echo.EchoOutput

object Util {

  //val testDir = "daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/"
  val testDir = "/edu/illinois/ncsa/daffodil/CLI/"
  val outputDir = testDir + "output/"

  val isWindows = System.getProperty("os.name").toLowerCase().startsWith("windows")

  def getExpectedString(filename: String): String = {
    val rsrc = Misc.getRequiredResource(outputDir + filename)
    //val source = scala.io.Source.fromFile(outputDir + filename)
    val source = scala.io.Source.fromFile(rsrc)
    var lines = source.mkString.trim()
    source.close()
    if (isWindows) {
      return fileConvert(lines)
    } else {
      return lines
    }
  }

  def start(cmd: String, expectErr: Boolean = false): Expect = {
    val spawnCmd = if (isWindows) {
      "cmd /k" + cmdConvert(cmd)
    } else {
      "/bin/bash"
    }
    
    return getShell(cmd, spawnCmd, expectErr)
  }

  // This function will be used if you are providing two separate commands
  // and doing the os check on the 'front end' (not within this utility class)
  def startNoConvert(cmd: String): Expect = {
    val spawnCmd = if (isWindows) {
      "cmd /k" + cmd
    } else {
      "/bin/bash"
    }

    return getShell(cmd, spawnCmd)
  }

  def getShell(cmd: String, spawnCmd: String, expectErr: Boolean = false): Expect = {
    val process = Runtime.getRuntime().exec(spawnCmd)
    val inputStream = if (expectErr) {
      process.getErrorStream()
    } else {
      process.getInputStream()
    }
    val shell = new ExpectBuilder()
        .withInputs(inputStream)
	.withOutput(process.getOutputStream())
	.withEchoOutput(new EchoOutput() {
	        @Override
		def onReceive(input: Int, string: String) = {
		    print(string)
		}
		@Override
		def onSend(string: String) = {
		    //print(string)
		}
	})
	.withErrorOnTimeout(true)
	.build();
    if (!isWindows) {
      shell.send(cmd)
    }
    return shell
  }

  def cmdConvert(str: String): String = {
    var newstr = str.replaceAll("""\\n?""", "")
    newstr = newstr.replaceAll("/", """\\\\""")
    return newstr
  }

  def fileConvert(str: String): String = {
    var newstr = str.replaceAll("\\r\\n", "\n")
    return newstr
  }

}
