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

package org.apache.daffodil.core.externalvars

import scala.util.Success

import org.apache.daffodil.lib.xml._

import org.junit.Assert._
import org.junit.Test

class TestExternalVariables {

  @Test def testQNameForIndividualVars() = {
    // This test just verifies that we're getting back
    // the right values for the namespace and variable name.
    //
    // This is required functionality for when individual vars
    // are passed in via the CLI using the -D option.
    //
    val varWithNS = "{someNS}varWithNS"
    val varNoNS = "{}varNoNS"
    val varGuessNS = "varGuessNS"

    val Success(qWithNS) = QName.refQNameFromExtendedSyntax(varWithNS)
    val Success(qNoNS) = QName.refQNameFromExtendedSyntax(varNoNS)
    val Success(qGuessNS) = QName.refQNameFromExtendedSyntax(varGuessNS)

    assertEquals(NS("someNS"), qWithNS.namespace)
    assertEquals("varWithNS", qWithNS.local)
    assertEquals(varWithNS, qWithNS.toString)

    assertEquals(NoNamespace, qNoNS.namespace)
    assertEquals("varNoNS", qNoNS.local)
    assertEquals(varNoNS, qNoNS.toString)

    assertEquals(UnspecifiedNamespace, qGuessNS.namespace)
    assertEquals("varGuessNS", qGuessNS.local)
  }

}
