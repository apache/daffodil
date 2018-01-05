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

package org.apache.daffodil.tunables

import org.junit.Test
import org.apache.daffodil.CLI.Util
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof

class TestCLITunables2 {

  /**
   * Suppresses SDW messages.
   */
  @Test def test_CLI_Parsing_SuppressSDEWarnings1() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/org/apache/daffodil/CLI/suppressWarnTest.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      // note: 2>&1 is shell-speak for "connect stderr into stdout"
      val cmd = String.format("""echo "a,b" | %s parse -s %s -TsuppressSchemaDefinitionWarnings=all 2>&1""", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("""<ex:e1 xmlns:ex="http://example.com">
  <s1>a</s1>
  <s2>b
</s2>
</ex:e1>"""))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  /**
   * Will display SDW warnings. Does not set the tunable that suppresses them.
   */
  @Test def test_CLI_Parsing_SuppressSDEWarnings2() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/org/apache/daffodil/CLI/suppressWarnTest.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      // note: 2>&1 is shell-speak for "connect stderr into stdout"
      val cmd = String.format("""echo "a,b" | %s parse -s %s 2>&1""", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("""Schema Definition Warning"""))
      shell.expect(contains("""dfdl:lengthKind"""))
      shell.expect(contains("""delimited"""))
      shell.expect(contains("""dfdl:length"""))
      shell.expect(contains("""<ex:e1 xmlns:ex="http://example.com">
  <s1>a</s1>
  <s2>b
</s2>
</ex:e1>"""))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }
}
