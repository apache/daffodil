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

import org.apache.daffodil.grammar.primitives.BinaryIntegerKnownLength
import org.apache.daffodil.schema.annotation.props.gen.{ BitOrder, ByteOrder }

trait BinaryIntegerKnownLengthCodeGenerator {

  def binaryIntegerKnownLengthGenerateCode(g: BinaryIntegerKnownLength, cgState: CodeGeneratorState): Unit = {
    // For the time being this is a very limited back end.
    // So there are numerous restrictions to enforce.
    val e = g.e
    val fieldName = e.namedQName.local
    val lengthInBits: Long = {
      e.schemaDefinitionUnless(e.elementLengthInBitsEv.isConstant, "Runtime dfdl:length expressions not supported.")
      val len = e.elementLengthInBitsEv.constValue.get
      len
    }

    val byteOrder: ByteOrder = {
      e.schemaDefinitionUnless(e.byteOrderEv.isConstant, "Runtime dfdl:byteOrder expressions not supported.")
      val bo = e.byteOrderEv.constValue
      bo
    }

    // We eventually want to lift this restriction.
    if (e.bitOrder ne BitOrder.MostSignificantBitFirst)
      e.SDE("Only dfdl:bitOrder 'mostSignificantBitFirst' is supported.")
    if (e.alignmentValueInBits.intValue() % 8 != 0)
      e.SDE("Only alignment to 8-bit (1 byte) boundaries is supported.")

    // Use an unusual memory bit pattern (magic debug value) to mark fields as
    // uninitialized in case generated code fails to set them during parsing.
    val initialValue = lengthInBits match {
      case 8 => "0xCC"
      case 16 => "0xCCCC"
      case 32 => "0xCCCCCCCC"
      case 64 => "0xCCCCCCCCCCCCCCCC"
      case _ => e.SDE("Lengths other than 8, 16, 32, or 64 bits are not supported.")
    }
    val initStatement = s"    instance->$fieldName = $initialValue;"
    val conv = if (byteOrder eq ByteOrder.BigEndian) "be" else "le"
    val parseStatement =
      s"""    if (!error_msg)
         |    {
         |        char   buffer[sizeof(uint${lengthInBits}_t)];
         |        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
         |        if (count < sizeof(buffer))
         |        {
         |            error_msg = eof_or_error_msg(pstate->stream);
         |        }
         |        instance->$fieldName = ${conv}${lengthInBits}toh(*((uint${lengthInBits}_t *)(&buffer)));
         |    }""".stripMargin
    val unparseStatement =
      s"""    if (!error_msg)
         |    {
         |        union
         |        {
         |            char     c_val[sizeof(uint${lengthInBits}_t)];
         |            uint${lengthInBits}_t i_val;
         |        } buffer;
         |        buffer.i_val = hto${conv}${lengthInBits}(instance->$fieldName);
         |        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
         |        if (count < sizeof(buffer))
         |        {
         |            error_msg = eof_or_error_msg(ustate->stream);
         |        }
         |    }""".stripMargin
    cgState.addSimpleTypeStatements(initStatement, parseStatement, unparseStatement)
  }
}
