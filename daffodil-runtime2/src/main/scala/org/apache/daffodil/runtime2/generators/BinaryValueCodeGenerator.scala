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
import org.apache.daffodil.schema.annotation.props.gen.LengthKind
import org.apache.daffodil.util.Maybe.Nope

// Base trait which provides common code to generate C code for primitive value elements
trait BinaryValueCodeGenerator {

  // Generate C code for a primitive value element differently depending on how many times the element occurs.
  // Intended to be called by other traits which extend this trait, not directly by Runtime2CodeGenerator.
  def binaryValueGenerateCode(e: ElementBase, addField: String => Unit, validateFixed: String => Unit, cgState: CodeGeneratorState): Unit = {
    // For the time being this is a very limited back end.
    // So there are some restrictions to enforce.
    e.schemaDefinitionUnless(e.bitOrder eq BitOrder.MostSignificantBitFirst, "Only dfdl:bitOrder 'mostSignificantBitFirst' is supported.")
    e.schemaDefinitionUnless(e.maybeByteOrderEv == Nope || e.byteOrderEv.isConstant, "Runtime dfdl:byteOrder expressions not supported.")
    e.schemaDefinitionUnless(e.lengthKind == LengthKind.Prefixed || e.elementLengthInBitsEv.isConstant, "Runtime dfdl:length expressions not supported.")

    // Call the given partially applied function values with their remaining unbound argument (deref)
    val deref = if (cgState.hasArray) "[i]" else ""
    addField(deref)
    if (e.hasFixedValue && e.fixedValueAsString.nonEmpty) {
      validateFixed(deref)
    }
  }

  // Generate C code to initialize, parse, and unparse a primitive value element.  Will be replaced by
  // more specialized functions in other traits for boolean and hexBinary elements.
  protected def valueAddField(e: ElementBase, lengthInBits: Long, primType: String, deref: String, cgState: CodeGeneratorState): Unit = {
    val indent1 = if (cgState.hasChoice) INDENT else NO_INDENT
    val indent2 = if (deref.nonEmpty) INDENT else NO_INDENT
    val localName = e.namedQName.local
    val field = s"instance->$localName$deref"
    val conv = if (e.byteOrderEv.constValue eq ByteOrder.BigEndian) "be" else "le"
    val function = s"${conv}_$primType"

    val initERDStatement = ""
    val parseStatement =
      s"""$indent1$indent2    parse_$function(&$field, $lengthInBits, pstate);
         |$indent1$indent2    if (pstate->error) return;""".stripMargin
    val unparseStatement =
      s"""$indent1$indent2    unparse_$function($field, $lengthInBits, ustate);
         |$indent1$indent2    if (ustate->error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, parseStatement, unparseStatement)
  }

  // Generate C code to validate a primitive element against its fixed value.  Will be replaced by
  // a more specialized function in another trait for hexBinary elements.
  protected def valueValidateFixed(e: ElementBase, deref: String, cgState: CodeGeneratorState): Unit = {
    val indent1 = if (cgState.hasChoice) INDENT else NO_INDENT
    val indent2 = if (deref.nonEmpty) INDENT else NO_INDENT
    val localName = e.namedQName.local
    val field = s"instance->$localName$deref"
    val fixed = e.fixedValueAsString

    val initERDStatement = ""
    val parseStatement =
      s"""$indent1$indent2    parse_validate_fixed($field == $fixed, "$localName", pstate);
         |$indent1$indent2    if (pstate->error) return;""".stripMargin
    val unparseStatement =
      s"""$indent1$indent2    unparse_validate_fixed($field == $fixed, "$localName", ustate);
         |$indent1$indent2    if (ustate->error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, parseStatement, unparseStatement)
  }
}
