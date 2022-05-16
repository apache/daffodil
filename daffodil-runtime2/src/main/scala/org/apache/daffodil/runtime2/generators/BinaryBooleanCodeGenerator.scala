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

package org.apache.daffodil.runtime2.generators

import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder
import passera.unsigned.ULong

trait BinaryBooleanCodeGenerator extends BinaryValueCodeGenerator {

  // Called by Runtime2CodeGenerator to generate C code for a boolean element
  def binaryBooleanGenerateCode(e: ElementBase, cgState: CodeGeneratorState): Unit = {
    // Make sure we know how to read and write the boolean element
    Assert.invariant(e.binaryBooleanTrueRep.isEmpty || e.binaryBooleanTrueRep.getULong >= ULong(0))
    Assert.invariant(e.binaryBooleanFalseRep >= ULong(0))
    Assert.invariant(e.elementLengthInBitsEv.isConstant)

    val lengthInBits = e.elementLengthInBitsEv.constValue.get
    val initialValue = "true"
    val primType = "bool"
    val addField = booleanAddField(e, initialValue, lengthInBits, primType, _, cgState)
    val validateFixed = valueValidateFixed(e, _, cgState)

    binaryValueGenerateCode(e, addField, validateFixed)
  }

  // Generate C code to initialize, parse, and unparse a boolean element
  private def booleanAddField(e: ElementBase, initialValue: String, lengthInBits: Long, primType: String, deref: String, cgState: CodeGeneratorState): Unit = {
    val localName = e.namedQName.local
    val field = s"instance->$localName$deref"
    val conv = if (e.byteOrderEv.constValue eq ByteOrder.BigEndian) "be" else "le"
    val function = s"${conv}_$primType"

    val trueRep = if (e.binaryBooleanTrueRep.isDefined) e.binaryBooleanTrueRep.getULong else -1
    val falseRep = e.binaryBooleanFalseRep
    val unparseTrueRep = if (e.binaryBooleanTrueRep.isDefined) s"$trueRep" else s"~$falseRep"

    val initERDStatement = ""
    val initSelfStatement = s"    $field = $initialValue;"
    val parseStatement =
      s"""    parse_$function(&$field, $lengthInBits, $trueRep, $falseRep, pstate);
         |    if (pstate->error) return;""".stripMargin
    val unparseStatement =
      s"""    unparse_$function($field, $lengthInBits, $unparseTrueRep, $falseRep, ustate);
         |    if (ustate->error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, initSelfStatement, parseStatement, unparseStatement)
  }
}
