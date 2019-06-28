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

package org.apache.daffodil.debugger

import org.junit.Test
import org.apache.daffodil.CLI.Util
import net.sf.expectit.matcher.Matchers.contains
import net.sf.expectit.matcher.Matchers.allOf
import net.sf.expectit.matcher.Matchers.times

class TestCLIdebugger {

  val DAFFODIL_JAVA_OPTS = Map("DAFFODIL_JAVA_OPTS" -> "-Xms256m -Xmx2048m -Djline.terminal=jline.UnsupportedTerminal -Dfile.encoding=UTF-8")
  //  Dubugging tests were not executing under Windows and especially under Eclipse
  //  due to the use of a non-interactive console.
  //  Set the DAFFODIL_JAVA_OPTS environment variable for Debugger tests to specify
  //  the use of an unsupported terminal: -Djline.terminal=jline.UnsupportedTerminal
  //  Also added a Java option to specify the character encoding: -Dfile.encoding=UTF-8

  @Test def test_3385_CLI_Debugger_invalidExpressions() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")
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

  @Test def test_1591_CLI_Debugger_invalidCommandError() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")
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

  @Test def test_1335_CLI_Debugger_dataAndWrapLength() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input2.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("debug"))

      shell.sendLine("info data")
      shell.expect(contains("0~,~1~,~2~,~3~,~4~,~5~,~6~"))

      //      shell.sendLine("set dataLength 5")
      //      shell.sendLine("info data")
      //      shell.expect(contains("0,1,2"))

      shell.sendLine("set dataLength -938")
      shell.sendLine("info data")
      shell.expect(contains("0~,~1~,~2~,~3~,~4~,~5~,~6~"))

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
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")
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
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")
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
      shell.expect(contains("</tns:cell>"))

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
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input6.txt")
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
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input6.txt")
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
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input3.txt")
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
      shell.expect(contains("2: cell   { dfdl:occursIndex() mod 2 eq 0 }"))

      shell.sendLine("display info occursIndex")

      shell.sendLine("continue")
      shell.expect(contains("occursIndex: 1"))

      shell.sendLine("continue")
      shell.expect(contains("occursIndex: 2"))

      shell.sendLine("continue")
      shell.expect(contains("occursIndex: 3"))

      shell.sendLine("disable breakpoint 2")

      shell.sendLine("continue")
      shell.expect(contains("occursIndex: 5"))

      shell.sendLine("continue")
      shell.expect(contains("occursIndex: 7"))

      shell.sendLine("enable breakpoint 2")

      shell.sendLine("continue")
      shell.expect(contains("occursIndex: 8"))

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
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input3.txt")
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
      shell.expect(contains("1: cell   { xsd:string(.) eq '3' }"))

      shell.sendLine("info breakpoints")
      shell.expect(allOf(contains("breakpoints:"), contains("1: cell   { xsd:string(.) eq '3' }")))

      shell.sendLine("continue")
      shell.expect(contains("<tns:cell>3</tns:cell>\n      </tns:row>\n    </tns:matrix>"))
      shell.sendLine("continue")
      shell.expect(contains("<tns:cell>3</tns:cell>\n      </tns:row>\n    </tns:matrix>"))
      shell.sendLine("continue")
      shell.expect(contains("<tns:cell>3</tns:cell>\n      </tns:row>\n    </tns:matrix>"))

      shell.sendLine("continue")
      shell.expect(times(1, contains("<tns:cell>3</tns:cell>")))
      shell.expect(contains("<tns:cell>3</tns:cell>\n      </tns:row>\n    </tns:matrix>"))
      shell.sendLine("continue")
      shell.expect(times(1, contains("<tns:cell>3</tns:cell>")))
      shell.expect(contains("<tns:cell>3</tns:cell>\n      </tns:row>\n    </tns:matrix>"))

      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1338_CLI_Debugger_discriminatorInfo() {
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input5.txt")
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
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info infoset")
      shell.sendLine("break cell")

      shell.sendLine("continue")
      shell.expect(contains("</tns:cell>"))

      shell.sendLine("step")
      shell.sendLine("step")
      shell.sendLine("step")
      shell.sendLine("step")
      shell.expect(contains("<tns:cell>0</tns:cell>"))

      shell.sendLine("continue")
      shell.expect(contains("</tns:cell>"))

      shell.sendLine("step")
      shell.sendLine("step")
      shell.sendLine("step")
      shell.sendLine("step")
      shell.expect(contains("<tns:cell>1</tns:cell>"))

      shell.sendLine("delete breakpoint 1")
      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1329_CLI_Debugger_breakpointTesting2() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input2.txt")
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
      shell.expect(contains("1: cell   { dfdl:occursIndex() eq 3 }"))

      shell.sendLine("continue")
      shell.expect(contains("</tns:cell>"))

      shell.sendLine("step")
      shell.sendLine("step")
      shell.sendLine("step")
      shell.sendLine("step")
      shell.expect(contains("<tns:cell>2</tns:cell>")) // lacks tns: prefix because debugger explicitly strips them.

      shell.sendLine("continue")
      shell.expect(contains("<tns:cell>6</tns:cell>")) // has tns prefix because this is the final infoset, not the debugger printing this.
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Debugger_SDE_message() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input2.txt")
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
      shell.expect(allOf(contains("Schema Definition Error"), contains("{}cell"), contains("tns:cell")))

      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1330_CLI_Debugger_breakpointTesting3() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input2.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info occursIndex")
      shell.expect(contains("(debug)"))
      shell.sendLine("break cell")
      shell.expect(contains("(debug)"))
      shell.sendLine("info breakpoints")
      shell.expect(contains("1: cell"))

      shell.sendLine("continue")
      shell.expect(contains("occursIndex: 1"))

      shell.sendLine("continue")
      shell.expect(contains("occursIndex: 2"))

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
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input3.txt")
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
      shell.expect(contains("</tns:matrix>"))

      shell.sendLine("set infosetLines 4")
      shell.sendLine("continue")
      shell.expect(contains("..."))
      shell.expect(contains("<tns:cell>3</tns:cell>"))
      shell.expect(contains("</tns:matrix>"))

      shell.sendLine("set infosetLines 10")
      shell.sendLine("continue")
      shell.expect(contains("<tns:matrix"))

      shell.sendLine("set infosetLines -900")
      shell.sendLine("continue")
      shell.expect(contains("<tns:matrix"))
      shell.expect(contains("</tns:matrix>"))

      shell.sendLine("disable breakpoint 1")
      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1334_CLI_Debugger_infoBitPosition() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")
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
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input4.txt")
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
      shell.expect(contains("childIndex: 4"))

      shell.sendLine("disable breakpoint 1")
      shell.sendLine("continue")
      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1340_CLI_Debugger_infoPath() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")
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
      shell.expect(contains("matrixType::sequence[1]::row::LocalComplexTypeDef::sequence[1]::cell"))

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
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input2.txt")
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
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema_03.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input9.txt")
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
    val schemaFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/cli_schema_03.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input9.txt")
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
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input2.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("display info data")
      shell.sendLine("step")
      shell.expect(contains("│")) //  (0 to 0)
      shell.expect(contains("0~,~1~,~2~,~3~,~4~,~5~,~6~"))

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
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input2.txt")
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

  @Test def test_CLI_Debugger_delimiterStack() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input2.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)

      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("break row")
      shell.expect(contains("(debug)"))

      shell.sendLine("continue")
      shell.expect(contains("(debug)"))

      shell.sendLine("info delimiterStack")
      shell.expect(contains("""local:  %NL; (separator)"""))
      shell.expect(contains("(debug)"))

      shell.sendLine("break cell")
      shell.expect(contains("(debug)"))

      shell.sendLine("continue")
      shell.expect(contains("(debug)"))

      shell.sendLine("info delimiterStack")
      shell.expect(contains("""remote: %NL; (separator)"""))
      shell.expect(contains("""local:  , (separator)"""))

      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_CLI_Debugger_utf16_encoding() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/utf16schema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/hextest.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r e2 %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("info data")
      shell.expect(contains("\u240A"))

      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  @Test def test_1337_CLI_Debugger_info_infoset() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/charClassEntities.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt")
    val (testSchemaFile, testInputFile) = if (Util.isWindows) (Util.cmdConvert(schemaFile), Util.cmdConvert(inputFile)) else (schemaFile, inputFile)

    val shell = if (Util.isWindows) Util.start("", envp = DAFFODIL_JAVA_OPTS) else Util.start("")

    try {
      val cmd = String.format("%s -d parse -s %s -r matrix %s", Util.binPath, testSchemaFile, testInputFile)
      shell.sendLine(cmd)
      shell.expect(contains("(debug)"))

      shell.sendLine("info infoset")
      shell.expect(contains("No Infoset"))

      shell.sendLine("step")
      shell.sendLine("info infoset")
      shell.expect(contains("matrix"))

      shell.sendLine("quit")
    } finally {
      shell.close()
    }
  }

  /* See DFDL-1264
  @Test def test_3585_CLI_Debugger_simpleDebugger_unparse() {
    val schemaFile = Util.daffodilPath("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/generalSchema.dfdl.xsd")
    val inputFile = Util.daffodilPath("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input12.txt")
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
  }*/

}
