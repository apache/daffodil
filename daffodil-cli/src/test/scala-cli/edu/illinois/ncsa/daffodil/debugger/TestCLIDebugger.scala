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

package edu.illinois.ncsa.daffodil.debugger

import junit.framework.Assert._
import org.junit.Test
import scala.xml._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.xml.XMLUtils._
import edu.illinois.ncsa.daffodil.compiler.Compiler
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.CLI.Util
import edu.illinois.ncsa.daffodil.tdml.DFDLTestSuite
import java.io.File
import net.sf.expectit.Expect
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.allOf
import net.sf.expectit.matcher.Matchers.times

class TestCLIdebugger {

  val DAFFODIL_JAVA_OPTS = Map("DAFFODIL_JAVA_OPTS" -> "-Xms1024m -Xmx1024m -XX:MaxPermSize=256m -XX:ReservedCodeCacheSize=128m -Djline.terminal=jline.UnsupportedTerminal -Dfile.encoding=UTF-8")
  //  Dubugging tests were not executing under Windows and especially under Eclipse 
  //  due to the use of a non-interactive console. 
  //  Set the DAFFODIL_JAVA_OPTS environment variable for Debugger tests to specify 
  //  the use of an unsupported terminal: -Djline.terminal=jline.UnsupportedTerminal 
  //  Also added a Java option to specify the character encoding: -Dfile.encoding=UTF-8

  @Test def test_3385_CLI_Debugger_invalidExpressions() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)

      shell.expect(contains("(debug)"))

      shell.sendLine("eval (/invalid)")
      shell.expect(contains("error: expression evaluation failed: Schema Definition Error:"))
      shell.expect(contains("(debug)"))

      shell.sendLine("eval (func())")
      shell.expect(contains("error: expression evaluation failed: Schema Definition Error: Unsupported function:"))
      shell.expect(contains("(debug)"))

      shell.sendLine("eval (/invalid!)")
      shell.expect(contains("error: expression evaluation failed: Schema Definition Error:"))
      shell.expect(contains("(debug)"))

      shell.sendLine("eval (!)")
      shell.expect(contains("error: expression evaluation failed: Schema Definition Error:"))
      shell.expect(contains("(debug)"))

      shell.sendLine("eval (././.\\/)")
      shell.expect(contains("error: expression evaluation failed: Schema Definition Error:"))
      shell.expect(contains("(debug)"))

      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_3263_CLI_Debugger_occursBounds() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input8.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info occursBounds")
      shell.expect(contains("(debug)"))

      shell.sendLine("s")
      shell.expect(contains("occursBounds: occurs bounds not set"))

      shell.sendLine("complete")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_3266_CLI_Debugger_occursBounds_shortForm() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input8.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info ob")
      shell.expect(contains("(debug)"))
      shell.sendLine("info oc")
      shell.expect(contains("error: undefined info command: oc"))

      shell.sendLine("s")
      shell.expect(contains("occursBounds: occurs bounds not set"))

      shell.sendLine("complete")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1591_CLI_Debugger_invalidCommandError() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))
      shell.sendLine("garbage")
      shell.expect(contains("error: undefined command: garbage"))
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1336_CLI_Debugger_occursBounds() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input8.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r file %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info occursBounds")
      shell.sendLine("display info infoset")
      shell.expect(contains("(debug)"))

      shell.sendLine("break item")

      shell.sendLine("continue")
      shell.expect(contains("occursBounds: 5"))

      shell.sendLine("continue")
      shell.expect(contains("occursBounds: 5"))

      shell.sendLine("complete")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_3398_CLI_Debugger_occursBounds_2() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input11.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r file %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info occursBounds")
      shell.sendLine("display info infoset")
      shell.expect(contains("(debug)"))

      shell.sendLine("break title")
      shell.sendLine("break item")

      shell.sendLine("continue")
      shell.expect(contains("breakpoint 1: title"))
      shell.expect(contains("occursBounds: 1024")) //occursCount is not set, so default is used

      shell.sendLine("disable breakpoint 1")

      shell.sendLine("continue")
      shell.expect(contains("breakpoint 2: item"))
      shell.expect(contains("occursBounds: 3"))

      shell.sendLine("complete")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1335_CLI_Debugger_dataAndWrapLength() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("debug"))

      shell.sendLine("info data")
      shell.expect(contains("0,1,2,3,4,5,6"))

      //      shell.sendLine("set dataLength 5")
      //      shell.sendLine("info data")
      //      shell.expect(contains("0,1,2"))

      shell.sendLine("set dataLength -938")
      shell.sendLine("info data")
      shell.expect(contains("0,1,2,3,4,5,6"))

      //      shell.sendLine("set wrapLength 2")
      //      shell.sendLine("info data")
      //      shell.expect(contains("0,\n    1,\n    2,\n    3,\n    4,\n    5,\n    6\n"))

      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_982_CLI_Debugger_simpleDebugger() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))
      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1326_CLI_Debugger_displaysTesting() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display eval (.)")
      shell.sendLine("step")
      shell.expect(contains("matrix"))

      shell.sendLine("info displays")
      shell.expect(contains("1: eval (.)"))

      shell.sendLine("disable display 1")
      shell.sendLine("info displays")
      shell.expect(contains("1*: eval (.)"))
      shell.sendLine("step")
      shell.sendLine("enable display 1")

      shell.sendLine("step")
      shell.expect(contains("0"))

      shell.sendLine("delete display 1")
      shell.sendLine("step")

      shell.sendLine("enable display 1")
      shell.expect(contains("error: 1 is not a valid display id"))

      shell.sendLine("continue")
      shell.expect(contains("matrix"))
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1339_CLI_Debugger_removeHidden() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input6.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r e %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)

      shell.expect(contains("(debug)"))
      shell.sendLine("set removeHidden false")
      shell.sendLine("display info infoset")
      shell.sendLine("break g")
      shell.sendLine("continue")
      shell.expect(contains("<sneaky>5</sneaky>"))
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_3268_CLI_Debugger_removeHidden2() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input6.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r e %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)

      shell.expect(contains("(debug)"))
      shell.sendLine("set removeHidden false")
      shell.sendLine("display info infoset")
      shell.sendLine("break g")
      shell.sendLine("continue")
      shell.expect(contains("<sneaky>5</sneaky>"))
      shell.sendLine("continue")
      val result = shell.expect(contains("</ex:e>")).getBefore();
      assert(!result.contains("sneaky"))
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1331_CLI_Debugger_breakpointTesting4() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input3.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("break cell")
      shell.sendLine("break cell")

      shell.sendLine("condition 1 dfdl:occursIndex() mod 2 eq 1")
      shell.sendLine("condition 2 dfdl:occursIndex() mod 2 eq 0")

      shell.sendLine("info breakpoints")
      shell.expect(contains("2: cell   dfdl:occursIndex() mod 2 eq 0"))

      shell.sendLine("display info arrayIndex")

      shell.sendLine("continue")
      shell.expect(contains("arrayIndex: 1"))

      shell.sendLine("continue")
      shell.expect(contains("arrayIndex: 2"))

      shell.sendLine("continue")
      shell.expect(contains("arrayIndex: 3"))

      shell.sendLine("disable breakpoint 2")

      shell.sendLine("continue")
      shell.expect(contains("arrayIndex: 5"))

      shell.sendLine("continue")
      shell.expect(contains("arrayIndex: 7"))

      shell.sendLine("enable breakpoint 2")

      shell.sendLine("continue")
      shell.expect(contains("arrayIndex: 8"))

      shell.sendLine("disable breakpoint 1")
      shell.sendLine("disable breakpoint 2")

      shell.sendLine("continue")
      shell.expect(contains("<tns:cell>3</tns:cell>"))
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1463_CLI_Debugger_breakOnValueOfElement() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input3.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("set breakOnlyOnCreation false")
      shell.expect(contains("(debug)"))

      shell.sendLine("display info infoset")
      shell.expect(contains("(debug)"))

      shell.sendLine("break cell")
      shell.expect(contains("1: cell"))
      shell.sendLine("condition 1 xsd:string(.) eq '3'")
      shell.expect(contains("1: cell   xsd:string(.) eq '3'"))

      shell.sendLine("info breakpoints")
      shell.expect(allOf(contains("breakpoints:"), contains("1: cell   xsd:string(.) eq '3'")))

      shell.sendLine("continue")
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
      shell.sendLine("continue")
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))

      shell.sendLine("continue")
      shell.expect(times(1, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
      shell.sendLine("continue")
      shell.expect(times(1, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))

      shell.sendLine("continue")
      shell.expect(times(2, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
      shell.sendLine("continue")
      shell.expect(times(2, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))

      shell.sendLine("continue")
      shell.expect(times(3, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
      shell.sendLine("continue")
      shell.expect(times(3, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))

      shell.sendLine("continue")
      shell.expect(times(4, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
      shell.sendLine("continue")
      shell.expect(times(4, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))

      shell.sendLine("continue")
      shell.expect(times(5, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
      shell.sendLine("continue")
      shell.expect(times(5, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))

      shell.sendLine("continue")
      shell.expect(times(6, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
      shell.sendLine("continue")
      shell.expect(times(6, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))

      shell.sendLine("continue")
      shell.expect(times(7, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
      shell.sendLine("continue")
      shell.expect(times(7, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))

      shell.sendLine("continue")
      shell.expect(times(8, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
      shell.sendLine("continue")
      shell.expect(times(8, contains("<cell>3</cell>")))
      shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))

      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1338_CLI_Debugger_discriminatorInfo() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input5.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r Item2 %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("break e3")
      shell.sendLine("break e4")
      shell.sendLine("display info discriminator")

      shell.sendLine("continue")
      shell.expect(contains("discriminator: true"))

      shell.sendLine("continue")
      shell.expect(contains("discriminator: true"))

      shell.sendLine("continue")
      shell.expect(contains("<e4>400</e4>"))
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1328_CLI_Debugger_breakpointTesting() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info infoset")
      shell.sendLine("break cell")

      shell.sendLine("continue")
      shell.expect(contains("<cell/>"))

      shell.sendLine("step")
      shell.sendLine("step")
      shell.expect(contains("<cell>0</cell>"))

      shell.sendLine("continue")
      shell.expect(contains("<cell/>"))

      shell.sendLine("step")
      shell.sendLine("step")
      shell.expect(contains("<cell>1</cell>"))

      shell.sendLine("delete breakpoint 1")
      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1329_CLI_Debugger_breakpointTesting2() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info infoset")
      shell.sendLine("break cell")
      shell.sendLine("condition 1 dfdl:occursIndex() eq 3")

      shell.sendLine("info breakpoints")
      shell.expect(contains("1: cell   dfdl:occursIndex() eq 3"))

      shell.sendLine("continue")
      shell.expect(contains("<cell/>"))

      shell.sendLine("step")
      shell.sendLine("step")
      shell.expect(contains("<cell>2</cell>")) // lacks tns: prefix because debugger explicitly strips them.

      shell.sendLine("continue")
      shell.expect(contains("<tns:cell>6</tns:cell>")) // has tns prefix because this is the final infoset, not the debugger printing this.
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Debugger_SDE_message() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info infoset")
      shell.sendLine("break cell")
      shell.sendLine("condition 1 fn:count(../cell) eq 3") // ../cell is wrong. Needs to be ../tns:cell

      shell.sendLine("continue")
      shell.expect(allOf(contains("Schema Definition Error"), contains("{}cell"), contains("{http://www.example.org/example1/}cell")))

      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1330_CLI_Debugger_breakpointTesting3() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info arrayIndex")
      shell.expect(contains("(debug)"))
      shell.sendLine("break cell")
      shell.expect(contains("(debug)"))
      shell.sendLine("info breakpoints")
      shell.expect(contains("1: cell"))

      shell.sendLine("continue")
      shell.expect(contains("arrayIndex: 1"))

      shell.sendLine("continue")
      shell.expect(contains("arrayIndex: 2"))

      shell.sendLine("disable breakpoint 1")
      shell.sendLine("info breakpoints")
      shell.expect(contains("1*: cell"))

      shell.sendLine("info data")
      // shell.expect(contains("(2 to 2)"))
      shell.expect(contains("0~,~1~,~2~,~3~,~4~,~5~,~6~"))

      shell.sendLine("continue")
      shell.expect(contains("<tns:cell>6</tns:cell>"))

      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1333_CLI_Debugger_settingInfosetLines() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input3.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info infoset")
      shell.sendLine("set infosetLines 1")

      shell.sendLine("break cell")
      shell.sendLine("continue")
      shell.expect(contains("..."))
      shell.expect(contains("</matrix>"))

      shell.sendLine("set infosetLines 4")
      shell.sendLine("continue")
      shell.expect(contains("..."))
      shell.expect(contains("<cell>3</cell>"))
      shell.expect(contains("</matrix>"))

      shell.sendLine("set infosetLines 10")
      shell.sendLine("continue")
      shell.expect(contains("""<matrix>"""))

      shell.sendLine("set infosetLines -900")
      shell.sendLine("continue")
      shell.expect(contains("""<matrix>"""))
      shell.expect(contains("</matrix>"))

      shell.sendLine("disable breakpoint 1")
      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1334_CLI_Debugger_infoBitPosition() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info bitPosition")
      shell.sendLine("display info data")
      shell.sendLine("break cell")

      shell.sendLine("continue")
      shell.expect(contains("bitPosition: 0"))

      shell.sendLine("continue")
      shell.expect(contains("bitPosition: 16"))

      shell.sendLine("continue")
      shell.expect(contains("bitPosition: 32"))

      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1337_CLI_Debugger_childIndex() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input4.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("break cell")
      shell.sendLine("display info childIndex")
      shell.sendLine("display info infoset")

      shell.sendLine("continue")
      shell.expect(contains("childIndex: 1"))

      shell.sendLine("continue")
      shell.expect(contains("childIndex: 2"))

      shell.sendLine("continue")
      shell.expect(contains("childIndex: 1"))

      shell.sendLine("disable breakpoint 1")
      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1340_CLI_Debugger_infoPath() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)
    val output1 = Util.getExpectedString("output1.txt")

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("break cell")
      shell.sendLine("display info path")

      shell.sendLine("continue")
      shell.expect(contains("element.matrix::GlobalComplexTypeDef(matrixType)::sequence::element.row::LocalComplexTypeDef::sequence::element.cell"))

      shell.sendLine("delete breakpoint 1")
      shell.expect(contains("debug"))
      shell.sendLine("continue")
      shell.expect(contains(output1))
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1382_CLI_Debugger_dataAndWrapLength2() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("break cell")
      shell.sendLine("continue")
      shell.sendLine("info data")
      shell.expect(contains("0~,~1~,~2~,~3~,~4~,~5~,~6~"))

      //      shell.sendLine("set dataLength 2")
      //      shell.sendLine("info data")
      //      shell.expect(contains("0,"))

      shell.sendLine("set dataLength -938")
      shell.sendLine("info data")
      shell.expect(contains("0~,~1~,~2~,~3~,~4~,~5~,~6~"))

      //      shell.sendLine("set wrapLength 2")
      //      shell.sendLine("info data")
      //      shell.expect(contains("    0,"))
      //      shell.expect(contains("    1,"))
      //      shell.expect(contains("    2,"))
      //      shell.expect(contains("    3,"))
      //      shell.expect(contains("    4,"))
      //      shell.expect(contains("    5,"))
      //      shell.expect(contains("    6"))

      shell.sendLine("disable breakpoint 1")
      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1863_CLI_Debugger_groupIndex01() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema_03.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input9.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -r list -s %s %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info groupIndex")
      shell.sendLine("break price")
      shell.expect(contains("1: price"))
      shell.sendLine("break comment")
      shell.expect(contains("2: comment"))

      shell.sendLine("continue")
      shell.expect(contains("groupIndex: 2"))
      shell.sendLine("continue")
      shell.expect(contains("groupIndex: 4"))
      shell.sendLine("continue")
      shell.expect(contains("groupIndex: 2"))
      shell.sendLine("continue")
      shell.expect(contains("groupIndex: 4"))
      shell.sendLine("continue")
      shell.expect(contains("<ex:price>89.99</ex:price>"))
    } finally {
      shell.close()
    }
  }

  @Test def test_1029_CLI_Debugger_validation1() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema_03.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input9.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -r list -s %s %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info dne1")
      shell.expect(contains("error: undefined info command: dne1"))
      shell.sendLine("display info bitLimit dne2")
      shell.expect(contains("error: undefined info command: dne2"))
      shell.sendLine("display break")
      shell.expect(contains("error: undefined command: break"))
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_3258_CLI_Debugger_infodata() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info data")
      shell.sendLine("step")
      shell.expect(contains("│")) //  (0 to 0)
      shell.expect(contains("0,1,2,3,4,5,6"))

      shell.sendLine("break cell")
      shell.sendLine("condition 1 dfdl:occursIndex() eq 5")
      shell.sendLine("continue")

      // Gaak. Eclipse default font isn't monospaced. The visible space character is wider than a regular character!
      shell.expect(contains("""#                                  │                                    │
                               #    87654321  0011 2233 4455 6677 8899 aabb ccdd eeff  0~1~2~3~4~5~6~7~8~9~a~b~c~d~e~f~
                               #    00000000: 302c 312c 322c 332c 342c 352c 36         0~,~1~,~2~,~3~,~4~,~5~,~6~      """.stripMargin('#')))
      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_3264_CLI_Debugger_undefined_command() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display data")
      shell.expect(contains("error: undefined command: data"))

      shell.sendLine("set breakonfailure true")
      shell.expect(contains("error: undefined command: breakonfailure"))

      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_3585_CLI_Debugger_simpleDebugger_unparse() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section00/general/generalSchema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input12.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d unparse -s %s -r e1 %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))
      shell.sendLine("break e1")
      shell.expect(contains("1: e1"))
      shell sendLine ("continue")
      shell.expect(contains("Hello  breakpoint 1: e1"))
      shell.sendLine("info data")
      shell.expect(contains(
        """4865 6c6c 6f                             Hello"""))
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

}
