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
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.util.Maybe.Nope

// Base trait which provides common code to generate C code for primitive value elements
trait BinaryValueCodeGenerator {

  // Generate C code for a primitive value element by calculating how many times the element occurs
  // and applying the given partially applied function values the necessary number of times.  Intended
  // to be called by other traits which extend this trait, not directly by Runtime2CodeGenerator.
  def binaryValueGenerateCode(e: ElementBase, addField: String => Unit, validateFixed: String => Unit): Unit = {
    // For the time being this is a very limited back end.
    // So there are some restrictions to enforce.
    e.schemaDefinitionUnless(e.bitOrder eq BitOrder.MostSignificantBitFirst, "Only dfdl:bitOrder 'mostSignificantBitFirst' is supported.")
    e.schemaDefinitionUnless(e.maybeByteOrderEv == Nope || e.byteOrderEv.isConstant, "Runtime dfdl:byteOrder expressions not supported.")
    e.schemaDefinitionUnless(e.lengthKind == LengthKind.Prefixed || e.elementLengthInBitsEv.isConstant, "Runtime dfdl:length expressions not supported.")

    // Calculate how many times the element occurs in the data.
    val arraySize = e.occursCountKind match {
      case OccursCountKind.Fixed if e.maxOccurs > 1 => e.maxOccurs
      case OccursCountKind.Fixed if e.maxOccurs == 1 => 0
      case OccursCountKind.Implicit if e.minOccurs == 1 && e.maxOccurs == 1 => 0
      case _ => e.SDE("occursCountKind %s minOccurs %d maxOccurs %d is not supported in C code generator",
        e.occursCountKind.toString, e.minOccurs, e.maxOccurs)
    }

    // Call the given partially applied function values with their remaining unbound argument (deref).
    def addFieldValidateFixed(deref: String): Unit = {
      addField(deref)
      if (e.hasFixedValue && e.fixedValueAsString.nonEmpty) {
        validateFixed(deref)
      }
    }

    // Generate the C code as many times as the element occurs.
    if (arraySize > 0)
      for (i <- 0 until arraySize)
        addFieldValidateFixed(s"[$i]")
    else
      addFieldValidateFixed("")
  }

  // Generate C code to initialize, parse, and unparse a primitive value element.  Will be replaced by
  // more specialized functions in other traits for boolean and hexBinary elements.
  protected def valueAddField(e: ElementBase, initialValue: String, lengthInBits: Long, primType: String, deref: String, cgState: CodeGeneratorState): Unit = {
    val localName = e.namedQName.local
    val field = s"instance->$localName$deref"
    val conv = if (e.byteOrderEv.constValue eq ByteOrder.BigEndian) "be" else "le"
    val function = s"${conv}_$primType"

    val initERDStatement = ""
    val initSelfStatement = s"    $field = $initialValue;"
    val parseStatement =
      s"""    parse_$function(&$field, $lengthInBits, pstate);
         |    if (pstate->error) return;""".stripMargin
    val unparseStatement =
      s"""    unparse_$function($field, $lengthInBits, ustate);
         |    if (ustate->error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, initSelfStatement, parseStatement, unparseStatement)
  }

  // Generate C code to validate a primitive element against its fixed value.  Will be replaced by
  // a more specialized function in another trait for hexBinary elements.
  protected def valueValidateFixed(e: ElementBase, deref: String, cgState: CodeGeneratorState): Unit = {
    val localName = e.namedQName.local
    val field = s"instance->$localName$deref"
    val fixed = e.fixedValueAsString

    val initERDStatement = ""
    val initSelfStatement = ""
    val parseStatement =
      s"""    parse_validate_fixed($field == $fixed, "$localName", pstate);
         |    if (pstate->error) return;""".stripMargin
    val unparseStatement =
      s"""    unparse_validate_fixed($field == $fixed, "$localName", ustate);
         |    if (ustate->error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, initSelfStatement, parseStatement, unparseStatement)
  }
}
