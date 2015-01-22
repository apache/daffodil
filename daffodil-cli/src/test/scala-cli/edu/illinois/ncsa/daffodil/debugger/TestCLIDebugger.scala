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
import net.sf.expectit.Expect
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.allOf
import net.sf.expectit.matcher.Matchers.times

class TestCLIdebugger {

  @Test def test_1591_CLI_Debugger_invalidCommandError() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))
    shell.send("garbage\n")
    shell.expect(contains("error: undefined command: garbage"))
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1336_CLI_Debugger_occursBounds() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r file daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input8.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("display info occursBounds\n")
    shell.send("display info infoset\n")
    shell.expect(contains("(debug)"))

    shell.send("break item\n")

    shell.send("continue\n")
    shell.expect(contains("occursBounds: 5"))

    shell.send("continue\n")
    shell.expect(contains("occursBounds: 5"))

    shell.send("complete\n")
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1335_CLI_Debugger_dataAndWrapLength() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("debug"))

    shell.send("info data\n")
    shell.expect(contains("0,1,2,3,4,5,6\n"))

    shell.send("set dataLength 5\n")
    shell.send("info data\n")
    shell.expect(contains("0,1,2\n"))

    shell.send("set dataLength -938\n")
    shell.send("info data\n")
    shell.expect(contains("0,1,2,3,4,5,6\n"))

    shell.send("set wrapLength 2\n")
    shell.send("info data\n")
    shell.expect(contains("0,\n    1,\n    2,\n    3,\n    4,\n    5,\n    6\n"))
    
    shell.send("continue")
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_982_CLI_Debugger_simpleDebugger() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))
    shell.send("continue\n")
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1326_CLI_Debugger_displaysTesting() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("display eval (.)\n")

    shell.send("step\n")
    shell.expect(contains("matrix"))

    shell.send("info displays\n")
    shell.expect(contains("1: eval (.)"))

    shell.send("disable display 1\n")
    shell.send("info displays\n")
    shell.expect(contains("1*: eval (.)"))
    shell.send("step\n")
    shell.send("enable display 1\n")

    shell.send("step\n")
    shell.expect(contains("0"))

    shell.send("delete display 1\n")
    shell.send("step\n")

    shell.send("enable display 1\n")
    shell.expect(contains("error: 1 is not a valid display id"))

    shell.send("continue\n")
    shell.expect(contains("matrix"))
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1339_CLI_Debugger_removeHidden() {
    val cmd = Util.binPath + " -d parse -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd -r e daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input6.txt\n"
    val shell = Util.start(cmd)

    shell.expect(contains("(debug)"))
    shell.send("set removeHidden false\n")
    shell.send("display info infoset\n")
    shell.send("break g\n")
    shell.send("continue\n")
    shell.expect(contains("<sneaky>5</sneaky>"))
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1331_CLI_Debugger_breakpointTesting4() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input3.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("break cell\n")
    shell.send("break cell\n")

    shell.send("condition 1 dfdl:occursIndex() mod 2 eq 1\n")
    shell.send("condition 2 dfdl:occursIndex() mod 2 eq 0\n")

    shell.send("info breakpoints\n")
    shell.expect(contains("2: cell   dfdl:occursIndex() mod 2 eq 0"))

    shell.send("display info arrayIndex\n")

    shell.send("continue\n")
    shell.expect(contains("arrayIndex: 1"))

    shell.send("continue\n")
    shell.expect(contains("arrayIndex: 2"))

    shell.send("continue\n")
    shell.expect(contains("arrayIndex: 3"))

    shell.send("disable breakpoint 2\n")

    shell.send("continue\n")
    shell.expect(contains("arrayIndex: 5"))

    shell.send("continue\n")
    shell.expect(contains("arrayIndex: 7"))

    shell.send("enable breakpoint 2\n")

    shell.send("continue\n")
    shell.expect(contains("arrayIndex: 8"))

    shell.send("disable breakpoint 1\n")
    shell.send("disable breakpoint 2\n")

    shell.send("continue\n")
    shell.expect(contains("<tns:cell>3</tns:cell>"))
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1463_CLI_Debugger_breakOnValueOfElement() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input3.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("set breakOnlyOnCreation false\n")
    shell.expect(contains("(debug)"))

    shell.send("display info infoset\n")
    shell.expect(contains("(debug)"))

    shell.send("break cell\n")
    shell.expect(contains("1: cell"))
    shell.send("condition 1 xsd:string(.) eq '3'\n")
    shell.expect(contains("1: cell   xsd:string(.) eq '3'"))

    shell.send("info breakpoints\n")
    shell.expect(allOf(contains("breakpoints:"), contains("1: cell   xsd:string(.) eq '3'")))

    shell.send("continue\n")
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    shell.send("continue\n")
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))

    shell.send("continue\n")
    shell.expect(times(1, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    shell.send("continue\n")
    shell.expect(times(1, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    
    shell.send("continue\n")
    shell.expect(times(2, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    shell.send("continue\n")
    shell.expect(times(2, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    
    shell.send("continue\n")
    shell.expect(times(3, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    shell.send("continue\n")
    shell.expect(times(3, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    
    shell.send("continue\n")
    shell.expect(times(4, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    shell.send("continue\n")
    shell.expect(times(4, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    
    shell.send("continue\n")
    shell.expect(times(5, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    shell.send("continue\n")
    shell.expect(times(5, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    
    shell.send("continue\n")
    shell.expect(times(6, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    shell.send("continue\n")
    shell.expect(times(6, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    
    shell.send("continue\n")
    shell.expect(times(7, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    shell.send("continue\n")
    shell.expect(times(7, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    
    shell.send("continue\n")
    shell.expect(times(8, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    shell.send("continue\n")
    shell.expect(times(8, contains("<cell>3</cell>")))
    shell.expect(contains("<cell>3</cell>\n      </row>\n    </matrix>"))
    
    shell.send("continue\n")
    shell.send("quit\n")
    shell.close()
  }
  
  @Test def test_1338_CLI_Debugger_discriminatorInfo() {
    val cmd = Util.binPath + " -d parse -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema.dfdl.xsd -r Item2 daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input5.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("break e3\n")
    shell.send("break e4\n")
    shell.send("display info discriminator\n")

    shell.send("continue\n")
    shell.expect(contains("discriminator: true"))

    shell.send("continue\n")
    shell.expect(contains("discriminator: true"))

    shell.send("continue\n")
    shell.expect(contains("<e4>400</e4>"))
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1328_CLI_Debugger_breakpointTesting() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("display info infoset\n")
    shell.send("break cell\n")

    shell.send("continue\n")
    shell.expect(contains("<cell/>"))

    shell.send("step\n")
    shell.send("step\n")
    shell.expect(contains("<cell>0</cell>"))

    shell.send("continue\n")
    shell.expect(contains("<cell/>"))

    shell.send("step\n")
    shell.send("step\n")
    shell.expect(contains("<cell>1</cell>"))

    shell.send("delete breakpoint 1\n")
    shell.send("continue\n")
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1329_CLI_Debugger_breakpointTesting2() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("display info infoset\n")
    shell.send("break cell\n")
    shell.send("condition 1 dfdl:occursIndex() eq 3\n")

    shell.send("info breakpoints\n")
    shell.expect(contains("1: cell   dfdl:occursIndex() eq 3"))

    shell.send("continue\n")
    shell.expect(contains("<cell/>"))

    shell.send("step\n")
    shell.send("step\n")
    shell.expect(contains("<cell>2</cell>"))

    shell.send("continue\n")
    shell.expect(contains("<tns:cell>6</tns:cell>"))
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1330_CLI_Debugger_breakpointTesting3() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("display info arrayIndex\n")
    shell.expect(contains("(debug)"))
    shell.send("break cell\n")
    shell.expect(contains("(debug)"))
    shell.send("info breakpoints\n")
    shell.expect(contains("1: cell"))

    shell.send("continue\n")
    shell.expect(contains("arrayIndex: 1"))

    shell.send("continue\n")
    shell.expect(contains("arrayIndex: 2"))

    shell.send("disable breakpoint 1\n")
    shell.send("info breakpoints\n")
    shell.expect(contains("1*: cell"))

    shell.send("info data\n")
    shell.expect(contains("(2 to 2)"))
    shell.expect(contains("0,1,2,3,4,5,6"))

    shell.send("continue\n")
    shell.expect(contains("<tns:cell>6</tns:cell>"))

    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1333_CLI_Debugger_settingInfosetLines() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input3.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("display info infoset\n")
    shell.send("set infosetLines 1\n")

    shell.send("break cell\n")
    shell.send("continue\n")
    shell.expect(contains("..."))
    shell.expect(contains("</matrix>"))

    shell.send("set infosetLines 4\n")
    shell.send("continue\n")
    shell.expect(contains("..."))
    shell.expect(contains("<cell>3</cell>"))
    shell.expect(contains("</matrix>"))

    shell.send("set infosetLines 10\n")
    shell.send("continue\n")
    shell.expect(contains("""<matrix>"""))

    shell.send("set infosetLines -900\n")
    shell.send("continue\n")
    shell.expect(contains("""<matrix>"""))
    shell.expect(contains("</matrix>"))

    shell.send("disable breakpoint 1\n")
    shell.send("continue\n")
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1334_CLI_Debugger_infoBitPosition() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("display info bitPosition\n")
    shell.send("display info data\n")
    shell.send("break cell\n")

    shell.send("continue\n")
    shell.expect(contains("bitPosition: 0"))

    shell.send("continue\n")
    shell.expect(contains("bitPosition: 16"))

    shell.send("continue\n")
    shell.expect(contains("bitPosition: 32"))

    shell.send("continue\n")
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1337_CLI_Debugger_childIndex() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input4.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("break cell\n")
    shell.send("display info childIndex\n")
    shell.send("display info infoset\n")

    shell.send("continue\n")
    shell.expect(contains("childIndex: 1"))

    shell.send("continue\n")
    shell.expect(contains("childIndex: 2"))

    shell.send("continue\n")
    shell.expect(contains("childIndex: 1"))

    shell.send("disable breakpoint 1\n")
    shell.send("continue\n")
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1340_CLI_Debugger_infoPath() {
    val output1 = Util.getExpectedString("output1.txt")
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input1.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("break cell\n")
    shell.send("display info path\n")

    shell.send("continue\n")
    shell.expect(contains("element.matrix::GlobalComplexTypeDef(matrixType)::sequence::element.row::LocalComplexTypeDef::sequence::element.cell"))

    shell.send("delete breakpoint 1\n")
    shell.expect(contains("debug"))
    shell.send("continue\n")
    shell.expect(contains(output1))
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1382_CLI_Debugger_dataAndWrapLength2() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("break cell\n")
    shell.send("continue\n")
    shell.send("info data\n")
    shell.expect(contains("0,1,2,3,4,5,6"))

    shell.send("set dataLength 2\n")
    shell.send("info data\n")
    shell.expect(contains("0,"))

    shell.send("set dataLength -938\n")
    shell.send("info data\n")
    shell.expect(contains("0,1,2,3,4,5,6"))

    shell.send("set wrapLength 2\n")
    shell.send("info data\n")
    shell.expect(contains("    0,"))
    shell.expect(contains("    1,"))
    shell.expect(contains("    2,"))
    shell.expect(contains("    3,"))
    shell.expect(contains("    4,"))
    shell.expect(contains("    5,"))
    shell.expect(contains("    6"))

    shell.send("disable breakpoint 1\n")
    shell.send("continue\n")
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_1863_CLI_Debugger_groupIndex01() {
    val cmd = Util.binPath + " -d parse -r list -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema_03.dfdl.xsd daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input9.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("display info groupIndex\n")
    shell.send("break price\n")
    shell.expect(contains("1: price"))
    shell.send("break comment\n")
    shell.expect(contains("2: comment"))
    
    shell.send("continue\n")
    shell.expect(contains("groupIndex: 2"))
    shell.send("continue\n")
    shell.expect(contains("groupIndex: 4"))
    shell.send("continue\n")
    shell.expect(contains("groupIndex: 2"))
    shell.send("continue\n")
    shell.expect(contains("groupIndex: 4"))
    shell.send("continue\n")
    shell.expect(contains("<ex:price>89.99</ex:price>"))
    shell.close()
  }

  @Test def test_1029_CLI_Debugger_validation1() {
    val cmd = Util.binPath + " -d parse -r list -s daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/cli_schema_03.dfdl.xsd daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input9.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("display info dne1\n")
    shell.expect(contains("error: undefined info command: dne1"))
    shell.send("display info bitLimit dne2\n")
    shell.expect(contains("error: undefined info command: dne2"))
    shell.send("display break\n")
    shell.expect(contains("error: undefined command: break"))
    shell.send("quit\n")
    shell.close()
  }

  @Test def test_3258_CLI_Debugger_infodata() {
    val cmd = Util.binPath + " -d parse -s daffodil-test/src/test/resources/edu/illinois/ncsa/daffodil/section06/entities/charClassEntities.dfdl.xsd -r matrix daffodil-cli/src/test/resources/edu/illinois/ncsa/daffodil/CLI/input/input2.txt\n"
    val shell = Util.start(cmd)
    shell.expect(contains("(debug)"))

    shell.send("display info data\n")
    shell.send("step\n")
    shell.expect(contains("│ (0 to 0)"))
    shell.expect(contains("0,1,2,3,4,5,6"))

    shell.send("break cell\n")
    shell.send("condition 1 dfdl:occursIndex() eq 5\n")
    shell.send("continue\n")

    shell.expect(contains("│ (8 to 8)"))
    shell.expect(contains("    4,5,6"))

    shell.send("continue\n")
    shell.send("quit\n")
    shell.close()
  }

}
