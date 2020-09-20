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
    // For the time being this is a very limited back end.
    // So there are numerous restrictions to enforce.
    e.schemaDefinitionUnless(lengthInBits <= 64, "Length must be 64 bits or less, but was: %s", lengthInBits)
    if (lengthInBits == 64 && !g.signed)
      e.SDE("Integers of 64 bits length must be signed.")

    // This insures we can use regular java.io library calls.
    if (e.alignmentValueInBits.intValue() % 8 != 0)
      e.SDE("Only alignment to 8-bit (1 byte) boundaries is supported.")

    // The restrictions below are ones we want to eventually lift.
    if (lengthInBits != 32)
      e.SDE("Lengths other than 32 bits are not supported.")

    if (byteOrder ne ByteOrder.BigEndian)
      e.SDE("Only dfdl:byteOrder 'bigEndian' is supported.")

    if (e.bitOrder ne BitOrder.MostSignificantBitFirst)
      e.SDE("Only dfdl:bitOrder 'mostSignificantBitFirst' is supported.")

    if (!g.signed)
      e.SDE("Only signed integers are supported.")

    val initStatement = s"    instance->$fieldName = 0xCDCDCDCD;"
    val parseStatement =
      s"""    if (!error_msg)
         |    {
         |        char   buffer[4];
         |        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
         |        if (count < sizeof(buffer))
         |        {
         |            error_msg = eof_or_error_msg(pstate->stream);
         |        }
         |        instance->$fieldName = be32toh(*((uint32_t *)(&buffer)));
         |    }""".stripMargin
    val unparseStatement =
      s"""    if (!error_msg)
         |    {
         |        union
         |        {
         |            char     c_val[4];
         |            uint32_t i_val;
         |        } buffer;
         |        buffer.i_val = htobe32(instance->$fieldName);
         |        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
         |        if (count < sizeof(buffer))
         |        {
         |            error_msg = eof_or_error_msg(ustate->stream);
         |        }
         |    }""".stripMargin
    cgState.addSimpleTypeStatements(initStatement, parseStatement, unparseStatement)
  }
}
