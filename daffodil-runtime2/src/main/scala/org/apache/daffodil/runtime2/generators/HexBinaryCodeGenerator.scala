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

import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.schema.annotation.props.gen.ByteOrder

trait HexBinaryCodeGenerator extends BinaryValueCodeGenerator {

  // Called by Runtime2CodeGenerator to generate C code for a hexBinary prefixed length element
  def hexBinaryLengthPrefixedGenerateCode(e: ElementBase, cgState: CodeGeneratorState): Unit = {
    val addField = hexBinaryPrefixedLengthAddField(e, _, cgState)
    val validateFixed = hexBinaryValidateFixed(e, _, cgState)

    binaryValueGenerateCode(e, addField, validateFixed, cgState)
  }

  // Called by Runtime2CodeGenerator to generate C code for a hexBinary specified length element
  def hexBinarySpecifiedLengthGenerateCode(e: ElementBase, cgState: CodeGeneratorState): Unit = {
    val addField = hexBinarySpecifiedLengthAddField(e, _, cgState)
    val validateFixed = hexBinaryValidateFixed(e, _, cgState)

    binaryValueGenerateCode(e, addField, validateFixed, cgState)
  }

  // Generate C code to initialize, parse, and unparse a hexBinary prefixed length element
  private def hexBinaryPrefixedLengthAddField(e: ElementBase, deref: String, cgState: CodeGeneratorState): Unit = {
    val indent1 = if (cgState.hasChoice) INDENT else NO_INDENT
    val indent2 = if (deref.nonEmpty) INDENT else NO_INDENT
    val localName = e.namedQName.local
    val field = s"instance->$localName$deref"
    val intType = e.prefixedLengthElementDecl.optPrimType.get match {
      case PrimType.Byte
           | PrimType.Short
           | PrimType.Int
           | PrimType.Long
           | PrimType.Integer => "int"
      case PrimType.UnsignedByte
           | PrimType.UnsignedShort
           | PrimType.UnsignedInt
           | PrimType.UnsignedLong
           | PrimType.NonNegativeInteger => "uint"
      case p => e.SDE("Prefixed length PrimType %s is not supported in C code generator.", p.toString)
    }
    val intLen = e.prefixedLengthElementDecl.elementLengthInBitsEv.constValue.get
    val primType = s"$intType$intLen"
    val conv = if (e.prefixedLengthElementDecl.byteOrderEv.constValue == ByteOrder.BigEndian) "be" else "le"
    val function = s"${conv}_$primType"
    val lenVar = s"_l_$localName"

    val initERDStatement =
      s"""$indent1$indent2    $field.dynamic = true;""".stripMargin
    val parseStatement =
      s"""$indent1$indent2    ${primType}_t $lenVar;
         |$indent1$indent2    parse_$function(&$lenVar, $intLen, pstate);
         |$indent1$indent2    if (pstate->error) return;
         |$indent1$indent2    alloc_hexBinary(&$field, $lenVar, pstate);
         |$indent1$indent2    if (pstate->error) return;
         |$indent1$indent2    parse_hexBinary(&$field, pstate);
         |$indent1$indent2    if (pstate->error) return;""".stripMargin
    val unparseStatement =
      s"""$indent1$indent2    unparse_$function($field.lengthInBytes, $intLen, ustate);
         |$indent1$indent2    if (ustate->error) return;
         |$indent1$indent2    unparse_hexBinary($field, ustate);
         |$indent1$indent2    if (ustate->error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, parseStatement, unparseStatement)
  }

  // Generate C code to initialize, parse, and unparse a hexBinary specified length element
  private def hexBinarySpecifiedLengthAddField(e: ElementBase, deref: String, cgState: CodeGeneratorState): Unit = {
    val indent1 = if (cgState.hasChoice) INDENT else NO_INDENT
    val indent2 = if (deref.nonEmpty) INDENT else NO_INDENT
    val localName = e.namedQName.local
    val field = s"instance->$localName$deref"
    val fieldArray = s"instance->_a_$localName$deref"
    val specifiedLength = e.elementLengthInBitsEv.constValue.get

    val initERDStatement = if (specifiedLength > 0)
      s"""$indent1$indent2    $field.array = $fieldArray;
         |$indent1$indent2    $field.lengthInBytes = sizeof($fieldArray);
         |$indent1$indent2    $field.dynamic = false;""".stripMargin
    else
      s"""$indent1$indent2    $field.array = NULL;
         |$indent1$indent2    $field.lengthInBytes = 0;
         |$indent1$indent2    $field.dynamic = false;""".stripMargin
    val parseStatement =
      s"""$indent1$indent2    parse_hexBinary(&$field, pstate);
         |$indent1$indent2    if (pstate->error) return;""".stripMargin
    val unparseStatement =
      s"""$indent1$indent2    unparse_hexBinary($field, ustate);
         |$indent1$indent2    if (ustate->error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, parseStatement, unparseStatement)
  }

  // Generate C code to validate a hexBinary element against its fixed value
  private def hexBinaryValidateFixed(e: ElementBase, deref: String, cgState: CodeGeneratorState): Unit = {
    val indent1 = if (cgState.hasChoice) INDENT else NO_INDENT
    val indent2 = if (deref.nonEmpty) INDENT else NO_INDENT
    val localName = e.namedQName.local
    val field = s"instance->$localName$deref"
    val fixed = s"${localName}_fixed"
    val array = e.fixedValueAsString.grouped(2).mkString("0x", ", 0x", "")

    val initERDStatement = ""
    val parseStatement =
      s"""$indent1$indent2    uint8_t $fixed[] = {$array};
         |$indent1$indent2    parse_validate_fixed(memcmp($field.array, $fixed, sizeof($fixed)) == 0, "$localName", pstate);
         |$indent1$indent2    if (pstate->error) return;""".stripMargin
    val unparseStatement =
      s"""$indent1$indent2    uint8_t $fixed[] = {$array};
         |$indent1$indent2    unparse_validate_fixed(memcmp($field.array, $fixed, sizeof($fixed)) == 0, "$localName", ustate);
         |$indent1$indent2    if (ustate->error) return;""".stripMargin
    cgState.addSimpleTypeStatements(initERDStatement, parseStatement, unparseStatement)
  }
}
