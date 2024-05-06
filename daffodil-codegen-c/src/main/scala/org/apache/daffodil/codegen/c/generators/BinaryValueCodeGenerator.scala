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

package org.apache.daffodil.codegen.c.generators

import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.schema.annotation.props.gen.ByteOrder
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType

// Base trait which provides common code to generate C code for primitive value elements
trait BinaryValueCodeGenerator {

  // Generate C code for a primitive value element differently depending on how many times the element occurs.
  // Intended to be called by other traits which extend this trait, not directly by DaffodilCCodeGenerator.
  def binaryValueGenerateCode(
    e: ElementBase,
    addField: String => Unit,
    validateFixed: String => Unit,
    cgState: CodeGeneratorState
  ): Unit = {
    // For the time being this is a very limited back end.
    // So there are some restrictions to enforce.
    e.schemaDefinitionUnless(
      e.bitOrder eq BitOrder.MostSignificantBitFirst,
      "Only dfdl:bitOrder 'mostSignificantBitFirst' is supported."
    )
    e.schemaDefinitionUnless(
      e.maybeByteOrderEv == Nope || e.byteOrderEv.isConstant,
      "Runtime dfdl:byteOrder expressions not supported."
    )

    // Call the given partially applied function values with their remaining unbound argument (deref)
    val deref = if (cgState.hasArray) "[i]" else ""
    addField(deref)
    if (e.hasFixedValue && e.fixedValueAsString.nonEmpty) {
      validateFixed(deref)
    }

    // Check if the element's value is restricted to a set of enumerations
    if (e.typeDef.optRestriction.exists(_.hasEnumeration)) {
      // Get the raw enumeration values to be inserted into the C code
      val enums = e.typeDef.optRestriction.get.enumerations.map(_.enumValueRaw)
      valueValidateEnumeration(e, deref, enums, cgState)
    }

    // Check if the element's value is restricted to a range (we will need to handle any
    // combination of inclusive and exclusive endpoints when generating our C expression)
    val hasMinExclusive = e.typeDef.optRestriction.exists(_.hasMinExclusive)
    val hasMinInclusive = e.typeDef.optRestriction.exists(_.hasMinInclusive)
    val hasMaxExclusive = e.typeDef.optRestriction.exists(_.hasMaxExclusive)
    val hasMaxInclusive = e.typeDef.optRestriction.exists(_.hasMaxInclusive)
    if (hasMinExclusive || hasMinInclusive || hasMaxExclusive || hasMaxInclusive) {
      // Generate the minimum endpoint comparison
      val minEndpoint = if (hasMinExclusive) {
        val endpoint = e.typeDef.optRestriction.map(_.minExclusiveValue).get
        s"""> $endpoint"""
      } else if (hasMinInclusive) {
        val endpoint = e.typeDef.optRestriction.map(_.minInclusiveValue).get
        e.optPrimType.get match {
          case PrimType.UnsignedByte | PrimType.UnsignedShort | PrimType.UnsignedInt |
              PrimType.UnsignedLong | PrimType.NonNegativeInteger if endpoint.toString == "0" =>
            // Avoid unsigned >= 0 comparisons (causes gcc warnings)
            ""
          case _ =>
            s""">= $endpoint"""
        }
      } else {
        ""
      }
      // Generate the maximum endpoint comparison
      val maxEndpoint = if (hasMaxExclusive) {
        val endpoint = e.typeDef.optRestriction.map(_.maxExclusiveValue).get
        s"""< $endpoint"""
      } else if (hasMaxInclusive) {
        val endpoint = e.typeDef.optRestriction.map(_.maxInclusiveValue).get
        s"""<= $endpoint"""
      } else {
        ""
      }
      // Call another function which can be redefined differently if necessary
      valueValidateRange(e, deref, minEndpoint, maxEndpoint, cgState)
    }
  }

  // Generate C code to parse and unparse a primitive value element.  Will be replaced by
  // more specialized functions in other traits for boolean and hexBinary elements.
  protected def valueAddField(
    e: ElementBase,
    lengthInBits: Long,
    primType: String,
    deref: String,
    cgState: CodeGeneratorState
  ): Unit = {
    val indent1 = if (cgState.hasChoice) INDENT else NO_INDENT
    val indent2 = if (deref.nonEmpty) INDENT else NO_INDENT
    val localName = cgState.cName(e)
    val field = s"instance->$localName$deref"
    val conv = if (e.byteOrderEv.constValue eq ByteOrder.BigEndian) "be" else "le"
    val function = s"${conv}_$primType"

    val initERDStatement = ""
    val parseStatement =
      s"""$indent1$indent2    parse_$function(&$field, $lengthInBits, pstate);
         |$indent1$indent2    if (pstate->pu.error) return;""".stripMargin
    val unparseStatement =
      s"""$indent1$indent2    unparse_$function($field, $lengthInBits, ustate);
         |$indent1$indent2    if (ustate->pu.error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, parseStatement, unparseStatement)
  }

  // Generate C code to validate a primitive element against its fixed value.  Will be replaced by
  // a more specialized function in another trait for hexBinary elements.
  protected def valueValidateFixed(
    e: ElementBase,
    deref: String,
    cgState: CodeGeneratorState
  ): Unit = {
    val indent1 = if (cgState.hasChoice) INDENT else NO_INDENT
    val indent2 = if (deref.nonEmpty) INDENT else NO_INDENT
    val localName = cgState.cName(e)
    val field = s"instance->$localName$deref"
    val fixed = e.fixedValueAsString

    val initERDStatement = ""
    val parseStatement =
      s"""$indent1$indent2    validate_fixed_attribute($field == $fixed, "$localName", &pstate->pu);
         |$indent1$indent2    if (pstate->pu.error) return;""".stripMargin
    val unparseStatement =
      s"""$indent1$indent2    validate_fixed_attribute($field == $fixed, "$localName", &ustate->pu);
         |$indent1$indent2    if (ustate->pu.error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, parseStatement, unparseStatement)
  }

  // Generate C code to validate a primitive element matches one of its enumeration values.
  private def valueValidateEnumeration(
    e: ElementBase,
    deref: String,
    enums: Seq[String],
    cgState: CodeGeneratorState
  ): Unit = {
    val indent1 = if (cgState.hasChoice) INDENT else NO_INDENT
    val indent2 = if (deref.nonEmpty) INDENT else NO_INDENT
    val localName = cgState.cName(e)
    val field = s"instance->$localName$deref"

    // Extra initialization only for hexBinary enumerations
    val arraysName = s"arrays_$localName"
    val hexEnums = enums.zipWithIndex.map { case (s, index) =>
      s"{$arraysName[$index], ${s.length / 2}, false}"
    }
    val hexEnumsInit = enums.map(_.grouped(2).map("0x" + _).mkString("{", ", ", "}"))
    val hexEnumsLenMax = enums.map(_.length / 2).max
    val arraysInit =
      s"""$indent1$indent2    uint8_t $arraysName[][$hexEnumsLenMax] = ${hexEnumsInit.mkString(
          "{",
          ", ",
          "}"
        )};\n"""

    val (enumsInit, extraInit, primType, valType, fieldArg) = e.optPrimType.get match {
      case PrimType.Double | PrimType.Float => (enums, "", "double", "floatpt", field)
      case PrimType.HexBinary => (hexEnums, arraysInit, "HexBinary", "hexbinary", s"&$field")
      case _ => (enums, "", "int64_t", "integer", field)
    }

    // Initialization for all enumerations
    val enumsArray = s"enums_$localName"
    val varsInit = extraInit +
      s"""$indent1$indent2    $primType $enumsArray[] = ${enumsInit.mkString(
          "{",
          ", ",
          "}"
        )};\n"""

    val initERDStatement = ""
    val parseStatement = varsInit +
      s"""$indent1$indent2    validate_${valType}_enumeration($fieldArg, ${enums.length}, $enumsArray, "$localName", &pstate->pu);
         |$indent1$indent2    if (pstate->pu.error) return;""".stripMargin
    val unparseStatement = varsInit +
      s"""$indent1$indent2    validate_${valType}_enumeration($fieldArg, ${enums.length}, $enumsArray, "$localName", &ustate->pu);
         |$indent1$indent2    if (ustate->pu.error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, parseStatement, unparseStatement)
  }

  // Generate C code to validate a primitive element fits within its schema facets' range.
  // May be replaced by a more specialized function in another trait for some types.
  private def valueValidateRange(
    e: ElementBase,
    deref: String,
    minEndpoint: String,
    maxEndpoint: String,
    cgState: CodeGeneratorState
  ): Unit = {
    val indent1 = if (cgState.hasChoice) INDENT else NO_INDENT
    val indent2 = if (deref.nonEmpty) INDENT else NO_INDENT
    val localName = cgState.cName(e)
    val field = s"instance->$localName$deref"
    val comparison =
      if (minEndpoint.nonEmpty && maxEndpoint.nonEmpty)
        s"""$field $minEndpoint && $field $maxEndpoint"""
      else if (minEndpoint.nonEmpty)
        s"""$field $minEndpoint"""
      else
        s"""$field $maxEndpoint"""

    val initERDStatement = ""
    val parseStatement =
      s"""$indent1$indent2    validate_schema_range($comparison, "$localName", &pstate->pu);
         |$indent1$indent2    if (pstate->pu.error) return;""".stripMargin
    val unparseStatement =
      s"""$indent1$indent2    validate_schema_range($comparison, "$localName", &ustate->pu);
         |$indent1$indent2    if (ustate->pu.error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, parseStatement, unparseStatement)
  }
}
