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
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind

trait BinaryAbstractCodeGenerator {

  def binaryAbstractGenerateCode(e: ElementBase, initialValue: String, prim: String,
    parseArgs: String, unparseArgs: String, cgState: CodeGeneratorState): Unit = {

    // For the time being this is a very limited back end.
    // So there are some restrictions to enforce.
    e.schemaDefinitionUnless(e.bitOrder eq BitOrder.MostSignificantBitFirst, "Only dfdl:bitOrder 'mostSignificantBitFirst' is supported.")
    e.schemaDefinitionUnless(e.byteOrderEv.isConstant, "Runtime dfdl:byteOrder expressions not supported.")
    e.schemaDefinitionUnless(e.elementLengthInBitsEv.isConstant, "Runtime dfdl:length expressions not supported.")

    val fieldName = e.namedQName.local
    val byteOrder = e.byteOrderEv.constValue
    val conv = if (byteOrder eq ByteOrder.BigEndian) "be" else "le"
    val arraySize = if (e.occursCountKind == OccursCountKind.Fixed) e.maxOccurs else 0
    val fixed = e.xml.attribute("fixed")
    val fixedValue = if (fixed.isDefined) fixed.get.text else ""

    def addStatements(deref: String): Unit = {
      val initStatement = s"    instance->$fieldName$deref = $initialValue;"
      val parseStatement =
        s"""    parse_${conv}_$prim(&instance->$fieldName$deref, $parseArgs);
           |    if (pstate->error) return;""".stripMargin
      val unparseStatement =
        s"""    unparse_${conv}_$prim(instance->$fieldName$deref, $unparseArgs);
           |    if (ustate->error) return;""".stripMargin
      cgState.addSimpleTypeStatements(initStatement, parseStatement, unparseStatement)

      if (fixedValue.nonEmpty) {
        val init2 = ""
        val parse2 =
          s"""    parse_validate_fixed(instance->$fieldName$deref == $fixedValue, "$fieldName", pstate);
             |    if (pstate->error) return;""".stripMargin
        val unparse2 =
          s"""    unparse_validate_fixed(instance->$fieldName$deref == $fixedValue, "$fieldName", ustate);
             |    if (ustate->error) return;""".stripMargin
        cgState.addSimpleTypeStatements(init2, parse2, unparse2)
      }
    }
    if (arraySize > 0)
      for (i <- 0 until arraySize)
        addStatements(s"[$i]")
    else
      addStatements("")
  }
}
