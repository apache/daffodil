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
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind

trait BinaryFloatCodeGenerator {

  def binaryFloatGenerateCode(e: ElementBase, lengthInBits: Int, cgState: CodeGeneratorState): Unit = {
    // For the time being this is a very limited back end.
    // So there are some restrictions to enforce.
    assert(lengthInBits == 32 || lengthInBits == 64)
    val byteOrder: ByteOrder = {
      e.schemaDefinitionUnless(e.byteOrderEv.isConstant, "Runtime dfdl:byteOrder expressions not supported.")
      val bo = e.byteOrderEv.constValue
      bo
    }

    // Use a NAN to mark our field as uninitialized in case parsing or unparsing
    // fails to set the field.
    val fieldName = e.namedQName.local
    val float = if (lengthInBits == 32) "float" else "double"
    val conv = if (byteOrder eq ByteOrder.BigEndian) "be" else "le"
    val arraySize = if (e.occursCountKind == OccursCountKind.Fixed) e.maxOccurs else 0

    def addSimpleTypeStatements(deref: String): Unit = {
      val initStatement = s"    instance->$fieldName$deref = NAN;"
      val parseStatement = s"    parse_${conv}_$float(&instance->$fieldName$deref, pstate);"
      val unparseStatement = s"    unparse_${conv}_$float(instance->$fieldName$deref, ustate);"
      cgState.addSimpleTypeStatements(initStatement, parseStatement, unparseStatement)
    }
    if (arraySize > 0)
      for (i <- 0 until arraySize)
        addSimpleTypeStatements(s"[$i]")
    else
      addSimpleTypeStatements("")
  }
}
