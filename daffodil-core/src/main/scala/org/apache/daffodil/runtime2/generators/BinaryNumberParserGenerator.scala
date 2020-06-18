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
  elementName: String)
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

  private val impl = new BinaryIntegerKnownLengthParserGeneratorImpl(
      e, signed, lengthInBits, alignmentInBits, byteOrder, bitOrder, elementName)

  override def generateCode(cgState: CodeGeneratorState): CodeGeneratorState =
    impl.generateCode(cgState)
}

/**
 * Unit testable "impl" version of corresponding generator.
 *
 * Broken out for testing/study purposes.
 *
 * This is independent of
 * daffodil-runtime1 objects like LengthInBitsEv or ByteOrderEv that
 * we can't easily construct for unit testing purposes.
 */
private [generators]
class  BinaryIntegerKnownLengthParserGeneratorImpl(
  e: ThrowsSDE,
  signed: Boolean,
  lengthInBits: Long,
  alignmentInBits: Int,
  byteOrder: ByteOrder,
  bitOrder: BitOrder,
  elementName: String)
  extends ParserGenerator {

  override def generateCode(cgState: CodeGeneratorState): CodeGeneratorState = {

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

    /* This code uses Julian Feinauer's code gen library so as to be independent of
       * whether we are generating C/C++, or Java, or even python.
       *
       * But I don't understand it that well, so first cut I'm going to generate Java
       * code as text. Then we can look at what I've done and figure out how to refactor and
       * use Julian's library.
       *
       * Another good idea is to create a Scala DSL for generating this language.
       * That would let us write pseudo code in this DSL without having to keep track of
       * what is happening vs. what is generating.
       *
       * A DSL for SQL in Scala exists, and the idea is you write this quasi-SQL,
       * fairly naturally, but what happens really is synthesis (and execution) of real SQL.
       */
    val newCGState = new CodeGeneratorState()
    newCGState
  }
}
