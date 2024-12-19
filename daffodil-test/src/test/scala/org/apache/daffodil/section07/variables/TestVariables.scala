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

package org.apache.daffodil.section07.variables

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Ignore
import org.junit.Test

object TestVariables extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/variables/variables.tdml"
}

object TestVariables01 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section07/variables/variables_01.tdml"
}

class TestVariables extends TdmlTests {
  val tdmlSuite = TestVariables

  @Test def setVarAbsolutePath = test
  @Test def varAsSeparator = test

  @Test def setVar1 = test
  @Test def doubleSetErr = test
  @Test def multiSetErr = test
  // DFDL-1443 & DFDL-1448
  @Ignore @Test def setAfterReadErr = test
  @Test def varInstance_01 = test
  @Test def varInstance_02 = test
  @Test def varInstance_03 = test
  @Test def varInstance_04 = test
  @Test def varInstance_05 = test
  @Test def varInstance_06 = test
  @Test def varInstance_07 = test
  @Test def varInstance_08 = test
  @Test def varInstance_09 = test
  @Test def varInstance_10 = test
  @Test def varInstance_11 = test
  @Test def varInstance_12 = test
  @Test def varInstance_13 = test
  @Test def varInstance_14 = test
  @Test def varInstance_15 = test

  @Test def varDirection_1 = test
  @Test def varDirection_2 = test
  @Test def varDirection_3 = test
  @Test def varDirection_err1 = test
  @Test def varDirection_err2 = test
  @Test def varDirection_setVar1 = test
  @Test def varDirection_setVar2 = test
  @Test def varDirection_nvi1 = test
  @Test def varDirection_nvi2 = test

  @Test def defineVariable_nonConstantExpression = test
  @Test def defineVariable_nonConstantExpression_unp = test
  @Test def circular_defineVariable_err = test
  @Test def defineVariable_ref_noDefault_err = test
  @Test def defineVariable_nonConstantExpression_setVar_err = test
  @Test def defineVariable_nonConstantExpression_setVar_err2 = test

  // DAFFODIL-2444 - This test triggers an unhandled NoSuchElement exception, which if handled then runs into an Assert.invariant
  @Ignore @Test def defineVariable_ref_infoset_err = test

  @Test def setVarChoice = test
  @Test def unparse_setVarChoice = test
  @Test def setVarOnSeqAndElemRef = test
  @Test def unparse_setVarOnSeq = test
  @Test def setVarOnGroupRef = test
  @Test def unparse_setVarOnGroupRef = test
  @Test def setVarSimpleType = test

  @Test def setVarValAttribute = test
  @Test def setVarValAttribute2 = test
  @Test def setVarTypeMismatch = test
  @Test def setVarCurrVal = test
  @Test def setVarMismatchRelative = test
  @Test def setVarExpression = test
  @Test def setVarExpression2 = test
  @Test def setVarBadScope = test
  @Test def varAsSeparator2 = test
  @Test def setVarBadScope2 = test

  @Test def resetVar_01 = test
  @Test def resetVar_02 = test

  @Test def doubleEmptyDefault = test
  @Test def emptyDefault = test
  @Test def emptyDefault2 = test

  @Test def var_end_path = test
  @Test def var_in_path = test

  @Test def logical_default_values = test
  @Test def logical_default_values_err = test

  @Test def unsignedIntVarCast = test

  // DFDL-2354
  @Test def NVI_with_CDK1 = test

  // DFDL-2374
  @Test def variables_nilled_element = test

  // DFDL-2375
  @Test def multipleBranchesWithNoElementsSetVariableError = test

  @Test def escapeCharVars_01 = test
  @Test def escapeCharVars_02 = test

  @Test def multipleVarReadInPoU_01 = test

  @Test def nviInGroupDecl_01 = test

  @Test def backwardsDefines_01 = test

  @Test def variables2 = test
  @Test def variables3 = test
}

class TestVariables01 extends TdmlTests {
  val tdmlSuite = TestVariables01

  @Test def doubleSetErr_d = test
  @Test def setVar1_d = test
  // DFDL-1443 & DFDL-1448
  @Ignore @Test def setAfterReadErr_d = test
  @Test def setVar1_d_parse = test
  @Test def setVar1_d_unparse = test
}
