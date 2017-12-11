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

package edu.illinois.ncsa.daffodil.tunables

import org.junit.Test
import edu.illinois.ncsa.daffodil.CLI.Util
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.eof

class TestCLITunables {

  val unqualifiedPathStep01 = Util.getExpectedString("unqualified_path_step_01.txt")
  val unqualifiedPathStep02 = Util.getExpectedString("unqualified_path_step_02.txt")
  val unqualifiedPathStep03 = Util.getExpectedString("unqualified_path_step_03.txt")
  val unqualifiedPathStep04 = Util.getExpectedString("unqualified_path_step_04.txt")

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_noNamespace_test_01() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format("echo -n 12| %s parse -s %s -r test_01 -TunqualifiedPathStepPolicy=noNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep01))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_noNamespace_test_02() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo -n 12| %s parse -s %s -r test_02 -TunqualifiedPathStepPolicy=noNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      //shell.expect(contains(unqualifiedPathStep02))
      shell.expect(contains("Schema Definition Error"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_noNamespace_test_03() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo -n 12| %s parse -s %s -r test_03 -TunqualifiedPathStepPolicy=noNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      //shell.expect(contains(unqualifiedPathStep03))
      shell.expect(contains("Schema Definition Error"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_noNamespace_test_04() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format("echo -n 12| %s parse -s %s -r test_04 -TunqualifiedPathStepPolicy=noNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep04))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }



  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_defaultNamespace_test_01() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format("echo -n 12| %s parse -s %s -r test_01 -TunqualifiedPathStepPolicy=defaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep01))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_defaultNamespace_test_02() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format("echo -n 12| %s parse -s %s -r test_02 -TunqualifiedPathStepPolicy=defaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep02))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_defaultNamespace_test_03() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo -n 12| %s parse -s %s -r test_03 -TunqualifiedPathStepPolicy=defaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      //shell.expect(contains(unqualifiedPathStep03))
      shell.expect(contains("Schema Definition Error"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_defaultNamespace_test_04() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo -n 12| %s parse -s %s -r test_04 -TunqualifiedPathStepPolicy=defaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains("Schema Definition Error"))
      //shell.expect(contains(unqualifiedPathStep04))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }



  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_preferDefaultNamespace_test_01() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format("echo -n 12| %s parse -s %s -r test_01 -TunqualifiedPathStepPolicy=preferDefaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep01))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_preferDefaultNamespace_test_02() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format("echo -n 12| %s parse -s %s -r test_02 -TunqualifiedPathStepPolicy=preferDefaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep02))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_preferDefaultNamespace_test_03() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("", true)

    try {
      val cmd = String.format("echo -n 12| %s parse -s %s -r test_03 -TunqualifiedPathStepPolicy=preferDefaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      //shell.expect(contains(unqualifiedPathStep03))
      shell.expect(contains("Schema Definition Error"))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Parsing_unqualifiedPathStepPolicy_preferDefaultNamespace_test_04() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/unqualified_path_step.dfdl.xsd")
    val testSchemaFile = if (Util.isWindows) Util.cmdConvert(schemaFile) else schemaFile
    val shell = Util.start("")

    try {
      val cmd = String.format("echo -n 12| %s parse -s %s -r test_04 -TunqualifiedPathStepPolicy=preferDefaultNamespace", Util.binPath, testSchemaFile)
      shell.sendLine(cmd)
      shell.expect(contains(unqualifiedPathStep04))
      shell.sendLine("exit")
      shell.expect(eof)
    } finally {
      shell.close()
    }
  }


}
