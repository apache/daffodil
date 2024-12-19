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

package org.apache.daffodil.section00.general

/* This section00 is for testing general features of DFDL that are
 * not related to any specific requirement
 */

import java.nio.file.Paths

import org.apache.daffodil.core.util.TestUtils.compileSchema
import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests
import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.tdml.TDMLException

import org.junit.Assert.assertTrue
import org.junit.Test
import org.xml.sax.SAXParseException

object TestDisallowTdmlDocType extends TdmlSuite {
  // This TDML file has a DOCTYPE declaration, so we should fail to
  // load it. However, that happens lazily.
  val tdmlResource = "/org/apache/daffodil/section00/general/hasDocType.tdml"
}

class TestDisallowTdmlDocType extends TdmlTests {
  val tdmlSuite = TestDisallowTdmlDocType

  @Test def ignored = {
    val e = intercept[TDMLException] { test }
    val msg = e.getMessage()
    assertTrue(msg.contains("DOCTYPE is disallowed"))
    assertTrue(msg.contains("hasDocType.tdml"))
  }
}

object TestDisallowDocType extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section00/general/disallowDocTypes.tdml"
}

class TestDisallowDocType extends TdmlTests {
  val tdmlSuite = TestDisallowDocType

  @Test def configMustNotHaveDocType(): Unit = {
    val e = intercept[TDMLException] { test }
    val msg = e.getMessage()
    assertTrue(msg.contains("DOCTYPE is disallowed"))
    assertTrue(msg.contains("hasDocType.cfg"))
  }

  @Test def dfdlSchemaMustNotHaveDocType = test
  @Test def dfdlSchemaMustNotHaveDocTypeViaInclude = test
  @Test def dfdlSchemaMustNotHaveDocTypeViaImport = test
  @Test def infosetFileMustNotHaveDocType(): Unit = {
    val e = intercept[TDMLException] { test }
    val msg = e.getMessage()
    assertTrue(msg.contains("DOCTYPE is disallowed"))
    assertTrue(msg.contains("hasDocType-infoset.xml"))
  }

  @Test
  def testExternalVariablesFileMustNotHaveDocType(): Unit = {
    val testSchema = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" lengthKind="delimited"/>,
      <xs:element name="e1" type="xs:string"/>
    )
    val schString = testSchema.toString()
    var dp: DataProcessor = compileSchema(testSchema)
    val extVarURI = Misc.getRequiredResource(
      "org/apache/daffodil/section00/general/hasDocType-external-vars.xml"
    )
    val extVarFile = Paths.get(extVarURI).toFile
    assertTrue(extVarFile.exists)
    val e = intercept[SAXParseException] {
      dp.withExternalVariables(extVarFile)
    }
    val m = e.getMessage()
    val f = e.getSystemId()
    assertTrue(m.contains("DOCTYPE is disallowed"))
    assertTrue(f.contains("hasDocType-external-vars.xml"))
  }
}
