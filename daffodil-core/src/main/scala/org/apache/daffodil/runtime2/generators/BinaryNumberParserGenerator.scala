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

import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.schema.annotation.props.gen.{BitOrder, ByteOrder}
import org.apache.daffodil.processors.{ByteOrderEv, LengthInBitsEv}

class BinaryIntegerKnownLengthParserGenerator(
                                               e: ThrowsSDE,
                                               signed: Boolean,
                                               lengthInBitsEv: LengthInBitsEv,
                                               alignmentInBits: Int,
                                               byteOrderEv: ByteOrderEv,
                                               bitOrder: BitOrder,
                                               enclosingElementName: String,
                                               fieldName: String)
  extends ParserGenerator {

  val lengthInBits: Long = {
    e.schemaDefinitionUnless(lengthInBitsEv.isConstant, "Runtime dfdl:length expressions not supported.")
    val len = lengthInBitsEv.constValue.get
    len
  }

  val byteOrder: ByteOrder = {
        e.schemaDefinitionUnless(byteOrderEv.isConstant, "Runtime dfdl:byteOrder expressions not supported.")
    val bo = byteOrderEv.constValue
    bo
  }

  override def generateCode(cgState: CodeGeneratorState): Unit = {

    // For the time being this is a very limited back end.
    // So there are numerous restrictions to enforce.
    e.schemaDefinitionUnless(lengthInBits <= 64, "Length must be 64 bits or less, but was: %s", lengthInBits)
    if (lengthInBits == 64 && !signed)
      e.SDE("Integers of 64 bits length must be signed.")

    // This insures we can use regular java.io library calls.
    if (alignmentInBits % 8 != 0)
      e.SDE("Only alignment to 8-bit (1 byte) boundaries is supported.")

    // The restrictions below are ones we want to eventually lift.
    if (lengthInBits != 32)
      e.SDE("Lengths other than 32 bits are not supported.")

    if (byteOrder ne ByteOrder.BigEndian)
      e.SDE("Only dfdl:byteOrder 'bigEndian' is supported.")

    if (bitOrder ne BitOrder.MostSignificantBitFirst)
      e.SDE("Only dfdl:bitOrder 'mostSignificantBitFirst' is supported.")

    if (!signed)
      e.SDE("Only signed integers are supported.")

    val field = s"instance->$fieldName"
    val parseStatement =
      s"""	// Read 4 bytes from pstate->stream
         |	// should handle insufficient number of bytes
         |	char buffer[4];
         |	int count = fread(&buffer, sizeof(buffer), 1, pstate->stream);
         |	if (count < sizeof(buffer))
         |	{
         |		// error handling - what do we do?
         |		// longjmp to an error routine, push an error and print it, exit immediately?
         |	}
         |	$field = be32toh(*((uint32_t *)(&buffer)));""".stripMargin
    cgState.addParseStatement(parseStatement)

    val unparseStatement =
      s"""	// Fill 4-byte buffer and write it to ustate->stream
         |	union {
         |		char c_val[4];
         |		uint32_t i_val;
         |	} buffer;
         |	buffer.i_val = htobe32($field);
         |	int count = fwrite(buffer.c_val, sizeof(buffer), 1, ustate->stream);
         |	if (count < sizeof(buffer))
         |	{
         |		// error handling goes here...
         |	}""".stripMargin
    cgState.addUnparseStatement(unparseStatement)
  }
}
