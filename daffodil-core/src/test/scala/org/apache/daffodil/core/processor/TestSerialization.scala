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

package org.apache.daffodil.core.processor

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.channels.Channels

import org.apache.daffodil.core.compiler.Compiler

import org.junit.Assert.assertTrue
import org.junit.Test

class TestSerialization {

  /**
   * DAFFODIL-2803
   *
   * Check that warnings are not serialized when saving a parser.
   */
  @Test def test_stripWarnings() = {
    val schema =
      <schema
        xmlns="http://www.w3.org/2001/XMLSchema"
        xmlns:dfdl="http://www.ogf.org/dfdl/dfdl-1.0/"
        xmlns:ex="http://example.com"
        targetNamespace="http://example.com"
      >
        <include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <annotation>
          <!-- The invalid appinfo source generates a warning -->
          <appinfo source="http://www.ogf.org/dfdl/WRONG">
            <dfdl:format ref="ex:GeneralFormat"/>
          </appinfo>
        </annotation>
        <element name="root" type="string" dfdl:lengthKind="explicit" dfdl:length="1"/>
      </schema>

    val factory = Compiler().compileNode(schema)
    val processor = factory.onPath("/")
    assertTrue(!processor.getDiagnostics.isEmpty)

    val os = new ByteArrayOutputStream()
    val output = Channels.newChannel(os)
    processor.save(output)

    val is = new ByteArrayInputStream(os.toByteArray)
    val processor2 = Compiler().reload(is)

    assertTrue(processor2.getDiagnostics.isEmpty)
  }

}
