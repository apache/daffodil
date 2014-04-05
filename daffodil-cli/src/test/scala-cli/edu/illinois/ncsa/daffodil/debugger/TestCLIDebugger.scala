package edu.illinois.ncsa.daffodil.CLI.debugger

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
import expectj.ExpectJ
import expectj.Spawn

class TestCLIdebugger {

  // TODO: the 'send' commands are currently commented-out because ExpectJ does not seem to communicate
  // with the debugger on Windows. The commands are sent via an instructions file. The commands have been
  // left in this file so it is obvious which input goes with which output; also, if this issue is
  // resolved, the commands will be used again.
  
  @Test def test_1339_CLI_Debugger_removeHidden() { 
    val cmd = "./daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1339 parse -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd -r e daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input6.txt\n"
    val shell = Util.start(cmd)

    shell.expect("(debug)")
    shell.expect("<ex:sneaky>5</ex:sneaky>")
    shell.stop()
    shell.expectClose()
  }

  @Test def test_1591_CLI_Debugger_invalidCommandError() {
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1591 parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")
    shell.expect("error: undefined command: garbage")
    shell.stop()
    shell.expectClose()
  }

  @Test def test_1331_CLI_Debugger_breakpointTesting4() {
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1331 parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input3.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")

    //    shell.send("break element.cell\n")
    //    shell.send("break element.cell\n")

    //    shell.send("condition 1 dfdl:occursIndex() mod 2 = 1\n")
    //    shell.send("condition 2 dfdl:occursIndex() mod 2 = 0\n")

    //    shell.send("info breakpoints\n")
    shell.expect("2: element.cell   dfdl:occursIndex() mod 2 = 0")

    //    shell.send("display info arrayIndex\n")

    //    shell.send("continue\n")
    shell.expect("arrayIndex: 1")

    //    shell.send("continue\n")
    shell.expect("arrayIndex: 2")

    //    shell.send("continue\n")
    shell.expect("arrayIndex: 3")

    //    shell.send("disable breakpoint 2\n")

    //    shell.send("continue\n")
    shell.expect("arrayIndex: 5")

    //    shell.send("continue\n")
    shell.expect("arrayIndex: 7")

    //    shell.send("enable breakpoint 2\n")

    //    shell.send("continue\n")
    shell.expect("arrayIndex: 8")

    //    shell.send("disable breakpoint 1\n")
    //    shell.send("disable breakpoint 2\n")

    //    shell.send("continue\n")
    shell.expect("<tns:cell>3</tns:cell>")
    //    shell.send("quit\n")
    shell.stop()
    shell.expectClose()
  }
  /* 
  @Test def test_1335_CLI_Debugger_dataAndWrapLength() {
    val cmd = "daffodil-cli/target/start -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt\n"
    val shell = Util.start(cmd)
  }
*/

  @Test def test_1338_CLI_Debugger_discriminatorInfo() {
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1338 parse -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd -r Item2 daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input5.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")

    //    shell.send("break element.e3\n")
    //    shell.send("break element.e4\n")
    //    shell.send("display info discriminator\n")

    //    shell.send("continue\n")
    shell.expect("discriminator: true")

    //    shell.send("continue\n")
    shell.expect("discriminator: true")

    //    shell.send("continue\n")
    shell.expect("<ex:e4>400</ex:e4>")
    //    shell.send("quit\n")
    shell.stop()
    shell.expectClose()
  }

  @Test def test_982_CLI_Debugger_simpleDebugger() {
    val cmd = "./daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/982 parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")
    //    shell.send("continue\n")

    //    shell.send("quit\n")
    shell.stop()
    shell.expectClose()
  }

  @Test def test_1326_CLI_Debugger_displaysTesting() {
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1326 parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")

    //    shell.send("display eval (.)\n")

    //    shell.send("step\n")
    shell.expect("<tns:matrix")

    //    shell.send("info displays\n")
    shell.expect("1: eval (.)")

    //    shell.send("disable display 1\n")
    //    shell.send("step\n")
    //    shell.send("enable display 1\n")

    //    shell.send("step\n")
    shell.expect("<tns:matrix")

    //    shell.send("delete display 1\n")
    //    shell.send("step\n")

    //    shell.send("enable display 1\n")
    shell.expect("error: 1 is not a valid display id")

    //    shell.send("continue\n")
    shell.expect("<tns:matrix")
    //    shell.send("quit\n")
    shell.stop()
    shell.expectClose()
  }

  @Test def test_1328_CLI_Debugger_breakpointTesting() {
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1328 parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")

    //    shell.send("display info infoset\n")
    //    shell.send("break element.cell\n")

    //    shell.send("continue\n")
    //    shell.expect("<tns:cell></tns:cell>") // scala 2.9 output
    shell.expect("<tns:cell/>") // scala 2.10 output

    //    shell.send("step\n")
    //    shell.send("step\n")
    shell.expect("<tns:cell>0</tns:cell>")

    //    shell.send("continue\n")
    //    shell.expect("<tns:cell></tns:cell>") // scala 2.9 output
    shell.expect("<tns:cell/>") // scala 2.10 output

    //    shell.send("step\n")
    //    shell.send("step\n")
    shell.expect("<tns:cell>1</tns:cell>")

    //    shell.send("delete breakpoint 1\n")
    //    shell.send("continue\n")
    //    shell.send("quit\n")
    shell.stop()
    shell.expectClose()
  }

  @Test def test_1329_CLI_Debugger_breakpointTesting2() {
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1329 parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")

    //    shell.send("display info infoset\n")
    //    shell.send("break element.cell\n")
    //    shell.send("condition 1 dfdl:occursIndex() = 3\n")

    //    shell.send("info breakpoints\n")
    shell.expect("1: element.cell   dfdl:occursIndex() = 3")

    //    shell.send("continue\n")
    //    shell.expect("<tns:cell></tns:cell>") // scala 2.9 output
    shell.expect("<tns:cell/>") // scala 2.10 output

    //    shell.send("step\n")
    //    shell.send("step\n")
    shell.expect("<tns:cell>2</tns:cell>")

    //    shell.send("continue\n")
    shell.expect("<tns:cell>6</tns:cell>")
    //    shell.send("quit\n")
    shell.stop()
    shell.expectClose()
  }

  @Test def test_1330_CLI_Debugger_breakpointTesting3() {
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1330 parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")

    //  display info arrayIndex
    //  break element.cell
    //  info breakpoints
    shell.expect("1: element.cell")

    //  continue
    shell.expect("arrayIndex: 1")

    //  continue
    shell.expect("arrayIndex: 2")

    //  disable breakpoint 1
    //  info breakpoints
    shell.expect("1*: element.cell")

    //  info data
    shell.expect("(2 to 2)")
    //    shell.expect("0,1,2,3,4,5,6")

    //  continue
    shell.expect("<tns:cell>6</tns:cell>")

    //  quit
    shell.stop()
    shell.expectClose()
  }

  //  @Test def test_1331_CLI_Debugger_breakpointTesting4() {} //DFDL-600 CLI Debugger: Allow for duplicate breakpoints with different conditionals

  @Test def test_1333_CLI_Debugger_settingInfosetLines() {
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1333 parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input3.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")

    //    shell.send("display info infoset\n")
    //    shell.send("set infosetLines 1\n")

    //    shell.send("break element.cell\n")
    //    shell.send("continue\n")
    shell.expect("...")
    shell.expect("</tns:matrix>")

    //    shell.send("set infosetLines 4\n")
    //    shell.send("continue\n")
    shell.expect("...")
    shell.expect("<tns:cell>3</tns:cell>")
    shell.expect("</tns:matrix>")

    //    shell.send("set infosetLines 10\n")
    //    shell.send("continue\n")
    shell.expect("""<tns:matrix xmlns:tns="http://www.example.org/example1/">""")

    //    shell.send("set infosetLines -900\n")
    //    shell.send("continue\n")
    shell.expect("""<tns:matrix xmlns:tns="http://www.example.org/example1/">""")
    shell.expect("</tns:matrix>")

    //    shell.send("disable breakpoint 1\n")
    //    shell.send("continue\n")
    //    shell.send("quit\n")
    shell.stop()
    shell.expectClose()
  }

  @Test def test_1334_CLI_Debugger_infoBitPosition() {
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1334 parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")

    //    shell.send("display info bitPosition\n")
    //    shell.send("display info data\n")
    //    shell.send("break element.cell\n")

    //    shell.send("continue\n")
    shell.expect("bitPosition: 0")

    //    shell.send("continue\n")
    shell.expect("bitPosition: 16")

    //    shell.send("continue\n")
    shell.expect("bitPosition: 32")

    //    shell.send("continue\n")
    //    shell.send("quit\n")
    shell.stop()
    shell.expectClose()
  }

  //  @Test def test_1335_CLI_Debugger_dataAndWrapLength() - in scala-debug (DFDL-650 "info data broken")

  @Test def test_1337_CLI_Debugger_childIndex() {
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1337 parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input4.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")

    //    shell.send("break element.cell\n")
    //    shell.send("display info childIndex\n")
    //    shell.send("display info infoset\n")

    //    shell.send("continue\n")
    shell.expect("childIndex: 1")

    //    shell.send("continue\n")
    shell.expect("childIndex: 2")

    //    shell.send("continue\n")
    shell.expect("childIndex: 1")

    //    shell.send("disable breakpoint 1\n")
    //    shell.send("continue\n")
    //    shell.send("quit\n")
    shell.stop()
    shell.expectClose()
  }

  //  @Test def test_1338_CLI_Debugger_discriminatorInfo() {} - in scala-debug (DFDL-603 "CLI: Debugger 'descriminator' command should be 'discriminator'")

  //  @Test def test_1339_CLI_Debugger_removeHidden() {

  @Test def test_1340_CLI_Debugger_infoPath() {
    val output1 = Util.getExpectedString("output1.txt")
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1340 parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")

    //    shell.send("break element.cell\n")
    //    shell.send("display info path\n")

    //    shell.send("continue\n")
    shell.expect("element.matrix::GlobalComplexTypeDef(matrixType)::sequence::element.row::LocalComplexTypeDef::sequence::element.cell")

    //    shell.send("delete breakpoint 1\n")
    shell.expect("debug")
    //    shell.send("continue\n")
    shell.expect(output1)
    //    shell.send("quit\n")
    shell.stop()
    shell.expectClose()
  }

  @Test def test_1382_CLI_Debugger_dataAndWrapLength2() {
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1382 parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")

    //    shell.send("break element.cell\n")
    //    shell.send("continue\n")
    //    shell.send("info data\n")
    shell.expect("0,1,2,3,4,5,6")

    //    shell.send("set dataLength 2\n")
    //    shell.send("info data\n")
    shell.expect("0,")

    //    shell.send("set dataLength -938\n")
    //    shell.send("info data\n")
    shell.expect("0,1,2,3,4,5,6")

    //    shell.send("set wrapLength 2\n")
    //    shell.send("info data\n")
    shell.expect("1,")

    //    shell.send("disable breakpoint 1\n")
    //    shell.send("continue\n")
    //    shell.send("quit\n")
    shell.stop()
    shell.expectClose()
  }
  
  @Test def test_1863_CLI_Debugger_groupIndex01() {
    val cmd = "daffodil-cli/target/start -d daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/debugger/1863 parse -r list -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema_03.dfdl.xsd daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input9.txt\n"
    val shell = Util.start(cmd)
    shell.expect("(debug)")

    shell.expect("1: element.price")
    shell.expect("2: element.comment")
    shell.expect("groupIndex: 2")
    shell.expect("groupIndex: 4")
    shell.expect("groupIndex: 2")
    shell.expect("groupIndex: 4")
    shell.expect("<ex:price>89.99</ex:price>")
    shell.stop()
    shell.expectClose()
  }

}
