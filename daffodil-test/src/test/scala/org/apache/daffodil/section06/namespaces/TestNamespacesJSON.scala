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

package org.apache.daffodil.section06.namespaces

import org.junit.Test
import org.junit.AfterClass
import org.apache.daffodil.tdml.Runner
import org.apache.daffodil.util.LoggingDefaults
import org.apache.daffodil.util.LogLevel
import org.apache.daffodil.util.SchemaUtils
import org.apache.daffodil.util.TestUtils
import org.apache.daffodil.compiler.Compiler
import org.apache.daffodil.util.Misc
import org.apache.daffodil.api.URISchemaSource

object TestNamespacesJSON {
  val testDir = "/org/apache/daffodil/section06/namespaces/"
}
class TestNamespacesJSON {

  import TestNamespaces._

  @Test def test_combinations_04_JSON() {

    val infoset =
      <base:baseSeq4 xmlns:base="http://baseSchema.com" xmlns:b06="http://b06.com">
        <base:cElem>done</base:cElem>
        <b06:cElem>done</b06:cElem>
      </base:baseSeq4>
    val data = "foobar"
    val compiler = Compiler()
    val filename = testDir + "multi_base_06_valid.dfdl.xsd"
    val uri = Misc.getRequiredResource(filename)
    val source = URISchemaSource(uri)
    val pf = compiler.compileSource(source)
    val str = TestUtils.testPFUnparse(pf, infoset, data, false)
    println(str)
  }

}
