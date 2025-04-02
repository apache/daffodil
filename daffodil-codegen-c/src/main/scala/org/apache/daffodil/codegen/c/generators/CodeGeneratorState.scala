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

import java.util.regex.Pattern
import scala.collection.mutable

import org.apache.daffodil.core.dsom.Choice
import org.apache.daffodil.core.dsom.ElementBase
import org.apache.daffodil.core.dsom.GlobalComplexTypeDef
import org.apache.daffodil.core.dsom.GlobalElementDecl
import org.apache.daffodil.core.dsom.SchemaComponent
import org.apache.daffodil.lib.cookers.ChoiceBranchKeyCooker
import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.runtime1.dpath.NodeInfo.PrimType

/**
 * Builds up the state of generated code.
 */
class CodeGeneratorState(private val root: ElementBase) {
  private val elementsAlreadySeen = mutable.Map[String, ElementBase]()
  private val structs = new mutable.Stack[ComplexCGState]()
  private val prototypes = mutable.ArrayBuffer[String]()
  private val erds = mutable.ArrayBuffer[String]()
  private val finalStructs = mutable.ArrayBuffer[String]()
  private val finalImplementation = mutable.ArrayBuffer[String]()

  // Push a dummy topmost state to simplify code
  structs.push(new ComplexCGState(cStructName(root), root))

  // Returns true if the generator is currently processing an array
  def hasArray: Boolean = structs.nonEmpty && structs.top.inArray

  // Returns true if the generator is currently processing a choice
  def hasChoice: Boolean = structs.nonEmpty && structs.top.initChoiceStatements.nonEmpty

  // Starts generating an element
  def pushElement(context: ElementBase): Unit = {
    // Generate a choice statement case if the element is in a choice element
    if (hasChoice) {
      val position = context.position
      structs.top.initChoiceStatements ++= ChoiceBranchKeyCooker
        .convertConstant(context.choiceBranchKey, context, forUnparse = false)
        .map { key => s"    case $key:" }
      structs.top.initChoiceStatements += s"        instance->_choice = $position;"
      structs.top.parserStatements += s"    case $position:"
      structs.top.unparserStatements += s"    case $position:"
    }

    if (context.isComplexType || context == root) {
      // Initialize complex element or distinguished root element which we treat like a complex element
      val C = cStructName(context)
      structs.push(new ComplexCGState(C, context))
      val erd = erdName(context)
      structs.top.initERDStatements +=
        s"""    instance->_base.erd = &$erd;
           |    instance->_base.parent = parent;""".stripMargin

      // Calculate padding if complex element has an explicit length
      if (
        context.isComplexType && context.maybeFixedLengthInBits.isDefined && context.maybeFixedLengthInBits.get > 0
      ) {
        val lengthInBits = context.maybeFixedLengthInBits.get
        structs.top.parserStatements += s"    const size_t end_bitPos0b = pstate->pu.bitPos0b + $lengthInBits;"
        structs.top.unparserStatements += s"    const size_t end_bitPos0b = ustate->pu.bitPos0b + $lengthInBits;"
      }
    }
  }

  // Finishes generating an element
  def popElement(context: ElementBase): Unit = {
    if (context.isComplexType) {
      // Calculate padding if complex element has an explicit length
      if (context.maybeFixedLengthInBits.isDefined && context.maybeFixedLengthInBits.get > 0) {
        structs.top.parserStatements +=
          s"""    parse_alignment_bits(end_bitPos0b, pstate);
             |    if (pstate->pu.error) return;""".stripMargin
        val octalFillByte = context.fillByteEv.constValue.toByte.toInt.toOctalString
        structs.top.unparserStatements +=
          s"""    unparse_alignment_bits(end_bitPos0b, '\\$octalFillByte', ustate);
             |    if (ustate->pu.error) return;""".stripMargin
      }

      // Finish generating the complex element's definition
      // while preventing redundant definitions on reused types
      if (elementNotSeenYet(context, cStructName(context))) {
        addComplexTypeERD(context) // ERD static initializer
        addStruct(context) // struct definition
        addImplementation(context)
      }
      structs.pop()

      // Generate statements for parsing complex elements
      val indent1 = if (hasChoice) INDENT else NO_INDENT
      val indent2 = if (hasArray) INDENT else NO_INDENT
      val C = cStructName(context)
      val e = cName(context)
      val deref = if (hasArray) "[i]" else ""
      if (hasChoice)
        structs.top.initChoiceStatements += s"$indent2        ${C}_initERD(&instance->$e$deref, (InfosetBase *)instance);"
      else
        structs.top.initERDStatements += s"$indent2    ${C}_initERD(&instance->$e$deref, (InfosetBase *)instance);"
      structs.top.parserStatements +=
        s"""$indent1$indent2    ${C}_parseSelf(&instance->$e$deref, pstate);
           |$indent1$indent2    if (pstate->pu.error) return;""".stripMargin
      structs.top.unparserStatements +=
        s"""$indent1$indent2    ${C}_unparseSelf(&instance->$e$deref, ustate);
           |$indent1$indent2    if (ustate->pu.error) return;""".stripMargin
    } else if (context == root) {
      // Treat a simple type root element as a hybrid of simple and complex types
      addFieldDeclaration(context) // struct member for element
      addComputations(context) // offset, ERD computations
      addSimpleTypeERD(context) // ERD static initializer
      addStruct(context) // struct definition
      addImplementation(context)
      structs.pop()
    } else {
      // Prevent redundant definitions on reused types
      if (elementNotSeenYet(context, cStructName(context))) {
        addSimpleTypeERD(context) // ERD static initializer
      }
    }

    // Link the element into its parent element
    addFieldDeclaration(context) // struct member for element
    addComputations(context) // offset, ERD computations

    // Generate a choice statement break if the child is in a choice element
    if (hasChoice) {
      val break = s"        break;"
      structs.top.initChoiceStatements += break
      structs.top.parserStatements += break
      structs.top.unparserStatements += break
    }
  }

  // Starts generating a reoccurring element (array element)
  def pushArray(context: SchemaComponent): Unit = {
    val C = structs.top.C
    val e = context.asInstanceOf[ElementBase]
    structs.push(new ComplexCGState(C, e, inArray = true))
  }

  // Finishes generating a reoccurring element (array element)
  def popArray(context: SchemaComponent): Unit = {
    // Finish generating array element
    val e = context.asInstanceOf[ElementBase]
    val C = structs.top.C
    val arrayName = s"array_${cStructName(e)}$C"
    // Prevent redundant definitions on reused types
    if (elementNotSeenYet(e, arrayName)) {
      addArrayImplementation(e)
    }

    // Link parent element to array element
    val declarations = structs.top.declarations
    val offsetComputations = structs.top.offsetComputations
    val erdComputations = structs.top.erdComputations
    structs.pop()
    structs.top.declarations ++= declarations
    structs.top.offsetComputations ++= offsetComputations
    structs.top.erdComputations ++= erdComputations

    // Now call the array's methods instead of the array's element's methods
    val indent = if (hasChoice) INDENT else NO_INDENT
    if (hasChoice)
      structs.top
        // TODO: not covered by tests
        .initChoiceStatements += s"$indent    ${arrayName}_initERD(instance, parent);"
    else
      structs.top.initERDStatements += s"$indent    ${arrayName}_initERD(instance, parent);"
    structs.top.parserStatements +=
      s"""$indent    ${arrayName}_parseSelf(instance, pstate);
         |$indent    if (pstate->pu.error) return;""".stripMargin
    structs.top.unparserStatements +=
      s"""$indent    ${arrayName}_unparseSelf(instance, ustate);
         |$indent    if (ustate->pu.error) return;""".stripMargin
  }

  // Generates choice member/ERD and switch statements for a choice group
  def addBeforeSwitchStatements(): Unit = {
    val context = structs.top.context
    val erd = erdName(context)
    val dispatchField = choiceDispatchField(context)
    if (dispatchField.nonEmpty) {
      val C = cStructName(context)
      val declaration =
        s"""    size_t      _choice; // choice of which union field to use
           |    union
           |    {""".stripMargin
      val erdDef =
        s"""static const ERD _choice_$erd = {
           |    {
           |        NULL, // namedQName.prefix
           |        "_choice", // namedQName.local
           |        NULL, // namedQName.ns
           |    },
           |    CHOICE, // typeCode
           |    0, NULL, NULL, NULL, NULL, {NULL}
           |};
           |""".stripMargin
      val offsetComputation =
        s"    (const char *)&${C}_compute_offsets._choice - (const char *)&${C}_compute_offsets"
      val erdComputation = s"    &_choice_$erd"
      val initChoiceStatement =
        s"""    static Error error = {ERR_CHOICE_KEY, {0}};
           |
           |    int64_t key = $dispatchField;
           |    switch (key)
           |    {""".stripMargin
      val parseStatement =
        s"""    static Error error = {ERR_CHOICE_KEY, {0}};
           |
           |    pstate->pu.error = instance->_base.erd->initChoice(&instance->_base);
           |    if (pstate->pu.error) return;
           |
           |    switch (instance->_choice)
           |    {""".stripMargin
      val unparseStatement =
        s"""    static Error error = {ERR_CHOICE_KEY, {0}};
           |
           |    ustate->pu.error = instance->_base.erd->initChoice(&instance->_base);
           |    if (ustate->pu.error) return;
           |
           |    switch (instance->_choice)
           |    {""".stripMargin

      // Prevent redundant definitions on reused types
      if (elementNotSeenYet(context, erd)) {
        erds += erdDef
      }
      structs.top.declarations += declaration
      structs.top.offsetComputations += offsetComputation
      structs.top.erdComputations += erdComputation
      structs.top.initChoiceStatements += initChoiceStatement
      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }
  }

  // Finishes generating a choice group's switch statements
  def addAfterSwitchStatements(): Unit = {
    if (hasChoice) {
      val declaration = s"    };"
      val initChoiceStatement =
        s"""    default:
           |        error.arg.d64 = key;
           |        return &error;
           |    }
           |
           |    return NULL;""".stripMargin
      val parseStatement =
        s"""    default:
           |        // Should never happen because initChoice would return an error first
           |        error.arg.d64 = (int64_t)instance->_choice;
           |        pstate->pu.error = &error;
           |        return;
           |    }""".stripMargin
      val unparseStatement =
        s"""    default:
           |        // Should never happen because initChoice would return an error first
           |        error.arg.d64 = (int64_t)instance->_choice;
           |        ustate->pu.error = &error;
           |        return;
           |    }""".stripMargin

      structs.top.declarations += declaration
      structs.top.initChoiceStatements += initChoiceStatement
      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }
  }

  // Adds C statements to initialize, parse, and unparse a primitive value element
  def addSimpleTypeStatements(
    initERDStatement: String,
    parseStatement: String,
    unparseStatement: String
  ): Unit = {
    if (initERDStatement.nonEmpty) {
      if (hasChoice)
        // TODO: not covered by tests
        structs.top.initChoiceStatements += initERDStatement
      else
        structs.top.initERDStatements += initERDStatement
    }
    if (parseStatement.nonEmpty) structs.top.parserStatements += parseStatement
    if (unparseStatement.nonEmpty) structs.top.unparserStatements += unparseStatement
  }

  // Generates a C header to define the Daffodil version
  def generateVersionHeader: String = {
    val daffodil = this.getClass.getPackage.getImplementationTitle
    val version = this.getClass.getPackage.getImplementationVersion
    val versionHeader =
      s"""#ifndef DAFFODIL_VERSION_H
         |#define DAFFODIL_VERSION_H
         |
         |const char *daffodil_version = "$daffodil $version";
         |
         |#endif // DAFFODIL_VERSION_H
         |""".stripMargin
    versionHeader.replace("\r\n", "\n").replace("\n", System.lineSeparator)
  }

  // Generates a C header to define the generated code
  def generateCodeHeader: String = {
    val structs = finalStructs.mkString("\n")
    val header =
      s"""#ifndef GENERATED_CODE_H
         |#define GENERATED_CODE_H
         |
         |// auto-maintained by iwyu
         |// clang-format off
         |#include <stdbool.h>  // for bool
         |#include <stddef.h>   // for size_t
         |#include <stdint.h>   // for uint8_t, int16_t, int32_t, int64_t, uint32_t, int8_t, uint16_t, uint64_t
         |#include "infoset.h"  // for InfosetBase, HexBinary
         |// clang-format on
         |
         |// Define schema version (will be empty if schema did not define any version string)
         |
         |extern const char *schema_version;
         |
         |// Define infoset structures
         |
         |$structs
         |#endif // GENERATED_CODE_H
         |""".stripMargin
    header.replace("\r\n", "\n").replace("\n", System.lineSeparator)
  }

  // Generates a C source file to implement the generated code
  def generateCodeFile: String = {
    val rootName = cStructName(root)
    val version = root.schemaDocument.version
    val prototypes = this.prototypes.mkString("\n")
    val erds = this.erds.mkString("\n")
    val finalImplementation = this.finalImplementation.mkString("\n")
    val code =
      s"""// auto-maintained by iwyu
         |// clang-format off
         |#include "generated_code.h"
         |#include <stdbool.h>    // for false, bool, true
         |#include <stddef.h>     // for NULL, size_t
         |#include <string.h>     // for memcmp, memset
         |#include "errors.h"     // for Error, PState, UState, ERR_CHOICE_KEY, Error::(anonymous), UNUSED
         |#include "parsers.h"    // for alloc_hexBinary, parse_hexBinary, parse_be_float, parse_be_int16, parse_be_bool32, parse_be_bool16, parse_be_int32, parse_be_uint16, parse_be_uint32, parse_le_bool32, parse_le_int64, parse_le_uint16, parse_le_uint8, parse_be_bool8, parse_be_double, parse_be_int64, parse_be_int8, parse_be_uint64, parse_be_uint8, parse_le_bool16, parse_le_bool8, parse_le_double, parse_le_float, parse_le_int16, parse_le_int32, parse_le_int8, parse_le_uint32, parse_le_uint64
         |#include "unparsers.h"  // for unparse_hexBinary, unparse_be_float, unparse_be_int16, unparse_be_bool32, unparse_be_bool16, unparse_be_int32, unparse_be_uint16, unparse_be_uint32, unparse_le_bool32, unparse_le_int64, unparse_le_uint16, unparse_le_uint8, unparse_be_bool8, unparse_be_double, unparse_be_int64, unparse_be_int8, unparse_be_uint64, unparse_be_uint8, unparse_le_bool16, unparse_le_bool8, unparse_le_double, unparse_le_float, unparse_le_int16, unparse_le_int32, unparse_le_int8, unparse_le_uint32, unparse_le_uint64
         |#include "validators.h" // for validate_array_bounds, validate_fixed_attribute, validate_floatpt_enumeration, validate_integer_enumeration, validate_schema_range
         |// clang-format on
         |
         |// Declare prototypes for easier compilation
         |
         |$prototypes
         |
         |// Define schema version (will be empty if schema did not define any version string)
         |
         |const char *schema_version = "$version";
         |
         |// Define metadata for the infoset
         |
         |$erds
         |// Initialize, parse, and unparse nodes of the infoset
         |
         |$finalImplementation
         |// Get an infoset (optionally clearing it first) for parsing/walking
         |
         |InfosetBase *
         |get_infoset(bool clear_infoset)
         |{
         |    static $rootName infoset;
         |
         |    if (clear_infoset)
         |    {
         |        // If your infoset contains hexBinary prefixed length elements,
         |        // you may want to walk infoset first to free their malloc'ed
         |        // storage - we are not handling that case for now...
         |        memset(&infoset, 0, sizeof(infoset));
         |        ${rootName}_initERD(&infoset, (InfosetBase *)&infoset);
         |    }
         |
         |    return &infoset._base;
         |}
         |""".stripMargin
    code.replace("\r\n", "\n").replace("\n", System.lineSeparator)
  }

  // Returns a C name for the given element's C struct identifier
  private def cStructName(context: ElementBase): String = {
    val sb = buildName(context, new StringBuilder)
    makeLegalForC(sb)
    val name = sb.toString
    name
  }

  // Returns a C name for the given element's element runtime data
  private def erdName(context: ElementBase): String = {
    val sb = buildName(context, new StringBuilder) ++= "ERD"
    makeLegalForC(sb)
    val name = sb.toString
    name
  }

  // Returns a C name for the given element's local name.
  def cName(context: ElementBase): String = {
    val sb = new StringBuilder(context.namedQName.local)
    makeLegalForC(sb)
    val name = sb.toString
    name
  }

  // Converts a dfdl:length expression to a C expression.  We make some
  // simplifying assumptions to make converting the expression easier:
  // - all field names start with ../ or /rootName
  // - all field names end at the first non-XML-identifier character
  // - all field names can be converted to C identifiers using cStructFieldAccess
  // - the expression performs only arithmetic (with no casts) and computes a length
  // - the expression is almost C-ready but may contain some non-C named operators
  // - may need to replace any div, idiv, mod with / and % instead
  // Eventually we should make the compiled expression generate the C code itself
  // instead of using this superficial replaceAllIn approach.
  def cExpression(expr: String): String = {
    // Match each field name and replace it with a C struct field dereference
    val field = """(((\.\./)+|/)[\p{L}_][\p{L}:_\-.0-9/]*)""".r
    val exprWithFields = field.replaceAllIn(
      expr,
      m => {
        val fieldName = m.group(1).stripPrefix("../")
        val cFieldName = cStructFieldAccess(fieldName)
        cFieldName
      }
    )
    // Match each named operator and replace it with a C operator
    val operator =
      """(\band\b|\bdiv\b|\beq\b|\bge\b|\bgt\b|\bidiv\b|\ble\b|\blt\b|\bmod\b|\bne\b|\bor\b)""".r
    val exprWithOperators = operator.replaceAllIn(
      exprWithFields,
      m => {
        val operatorName = m.group(1)
        val cOperatorSymbol = operatorName match {
          case "and" => "&&"
          case "div" => "/"
          case "eq" => "=="
          case "ge" => ">="
          case "gt" => ">"
          case "idiv" => "/"
          case "le" => "<="
          case "lt" => "<"
          case "mod" => "%"
          case "ne" => "!="
          case "or" => "||"
        }
        cOperatorSymbol
      }
    )
    exprWithOperators
  }

  // Adds an ERD definition for the given complex element
  private def addComplexTypeERD(context: ElementBase): Unit = {
    val C = cStructName(context)
    val erd = erdName(context)
    val count = structs.top.offsetComputations.length
    val offsetComputations = structs.top.offsetComputations.mkString(",\n")
    val erdComputations = structs.top.erdComputations.mkString(",\n")
    val qNameInit = defineQNameInit(context)
    val numChildren = if (hasChoice) 2 else count
    val initChoice = if (hasChoice) s"(InitChoiceRD)&${C}_initChoice" else "NULL"
    val complexERD =
      if (numChildren > 0)
        s"""static const $C ${C}_compute_offsets;
         |
         |static const size_t ${C}_childrenOffsets[$count] = {
         |$offsetComputations
         |};
         |
         |static const ERD *const ${C}_childrenERDs[$count] = {
         |$erdComputations
         |};
         |
         |static const ERD $erd = {
         |$qNameInit
         |    COMPLEX, // typeCode
         |    $numChildren, // numChildren
         |    ${C}_childrenOffsets,
         |    ${C}_childrenERDs,
         |    (ERDParseSelf)&${C}_parseSelf,
         |    (ERDUnparseSelf)&${C}_unparseSelf,
         |    {.initChoice = $initChoice}
         |};
         |""".stripMargin
      else
        s"""static const ERD $erd = {
         |$qNameInit
         |    COMPLEX, // typeCode
         |    $numChildren, // numChildren
         |    NULL, // childrenOffsets
         |    NULL, // childrenERDs
         |    (ERDParseSelf)&${C}_parseSelf,
         |    (ERDUnparseSelf)&${C}_unparseSelf,
         |    {.initChoice = $initChoice}
         |};
         |""".stripMargin

    erds += complexERD
  }

  // Adds a C struct definition for the given complex element
  private def addStruct(context: ElementBase): Unit = {
    val C = cStructName(context)
    val declarations = structs.top.declarations.mkString("\n")
    val struct =
      s"""typedef struct $C
         |{
         |    InfosetBase _base;
         |$declarations
         |} $C;
         |""".stripMargin

    finalStructs += struct
  }

  // Generates a complex element's initERD, parseSelf, unparseSelf functions
  private def addImplementation(context: ElementBase): Unit = {
    val C = cStructName(context)
    val initERDStatements = structs.top.initERDStatements.mkString("\n")
    val initChoiceStatements = structs.top.initChoiceStatements.mkString("\n")
    val parserStatements =
      if (structs.top.parserStatements.nonEmpty)
        structs.top.parserStatements.mkString("\n")
      else
        s"""    // Empty struct, but need to prevent compiler warnings
         |    UNUSED(instance);
         |    UNUSED(pstate);""".stripMargin
    val unparserStatements =
      if (structs.top.unparserStatements.nonEmpty)
        structs.top.unparserStatements.mkString("\n")
      else
        s"""    // Empty struct, but need to prevent compiler warnings
         |    UNUSED(instance);
         |    UNUSED(ustate);""".stripMargin
    val prototypeInitChoice =
      if (hasChoice)
        s"static const Error *${C}_initChoice($C *instance);\n"
      else
        ""
    val implementInitChoice =
      if (hasChoice)
        s"""
         |static const Error *
         |${C}_initChoice($C *instance)
         |{
         |$initChoiceStatements
         |}
         |""".stripMargin
      else
        ""
    val prototypeFunctions =
      s"""${prototypeInitChoice}static void ${C}_parseSelf($C *instance, PState *pstate);
         |static void ${C}_unparseSelf(const $C *instance, UState *ustate);""".stripMargin
    val functions =
      s"""static void
         |${C}_initERD($C *instance, InfosetBase *parent)
         |{
         |$initERDStatements
         |}
         |$implementInitChoice
         |static void
         |${C}_parseSelf($C *instance, PState *pstate)
         |{
         |$parserStatements
         |}
         |
         |static void
         |${C}_unparseSelf(const $C *instance, UState *ustate)
         |{
         |$unparserStatements
         |}
         |""".stripMargin

    prototypes += prototypeFunctions
    finalImplementation += functions
  }

  // Returns true if the element has not been seen before (checking if a
  // map already contains the element, otherwise adding it to the map)
  private def elementNotSeenYet(context: ElementBase, key: String): Boolean = {
    val alreadySeen = elementsAlreadySeen.contains(key)
    if (!alreadySeen)
      elementsAlreadySeen += (key -> context)
    !alreadySeen
  }

  // Adds an ERD definition for a simple element or simple root element
  private def addSimpleTypeERD(context: ElementBase): Unit = {
    val C = cStructName(context)
    val erd = erdName(context)
    val count = structs.top.offsetComputations.length
    val offsetComputations = structs.top.offsetComputations.mkString(",\n")
    val qNameInit = defineQNameInit(context)
    val typeCode = getPrimType(context) match {
      case PrimType.Boolean => "PRIMITIVE_BOOLEAN"
      case PrimType.Double => "PRIMITIVE_DOUBLE"
      case PrimType.Float => "PRIMITIVE_FLOAT"
      case PrimType.HexBinary => "PRIMITIVE_HEXBINARY"
      case PrimType.Short => "PRIMITIVE_INT16"
      case PrimType.Int => "PRIMITIVE_INT32"
      case PrimType.Long => "PRIMITIVE_INT64"
      case PrimType.Byte => "PRIMITIVE_INT8"
      case PrimType.UnsignedShort => "PRIMITIVE_UINT16"
      case PrimType.UnsignedInt => "PRIMITIVE_UINT32"
      case PrimType.UnsignedLong => "PRIMITIVE_UINT64"
      case PrimType.UnsignedByte => "PRIMITIVE_UINT8"
      case p => context.SDE("PrimType %s is not supported.", p.toString)
    }
    // Treat a simple type root element as a hybrid of simple and complex types
    val erdDef =
      if (context == root)
        s"""|static const $C ${C}_compute_offsets;
          |
          |static const size_t ${C}_childrenOffsets[$count] = {
          |$offsetComputations
          |};
          |
          |static const ERD $erd = {
          |$qNameInit
          |    $typeCode, // typeCode
          |    0, // numChildren
          |    ${C}_childrenOffsets,
          |    NULL, // childrenERDs
          |    (ERDParseSelf)&${C}_parseSelf,
          |    (ERDUnparseSelf)&${C}_unparseSelf,
          |    {.initChoice = NULL}
          |};
          |""".stripMargin
      else
        s"""|static const ERD $erd = {
          |$qNameInit
          |    $typeCode, // typeCode
          |    0, NULL, NULL, NULL, NULL, {NULL}
          |};
          |""".stripMargin

    erds += erdDef
  }

  // Adds a field declaration for an element to its parent element's struct
  private def addFieldDeclaration(child: ElementBase): Unit = {
    val definition = if (child.isSimpleType) {
      getPrimType(child) match {
        case PrimType.Boolean => "bool       "
        case PrimType.Double => "double     "
        case PrimType.Float => "float      "
        case PrimType.HexBinary => "HexBinary  "
        case PrimType.Short => "int16_t    "
        case PrimType.Int => "int32_t    "
        case PrimType.Long => "int64_t    "
        case PrimType.Byte => "int8_t     "
        case PrimType.UnsignedShort => "uint16_t   "
        case PrimType.UnsignedInt => "uint32_t   "
        case PrimType.UnsignedLong => "uint64_t   "
        case PrimType.UnsignedByte => "uint8_t    "
        case p => child.SDE("PrimType %s is not supported: ", p.toString)
      }
    } else {
      cStructName(child)
    }
    val e = cName(child)
    val arraySize = arrayMaxOccurs(child)
    val arrayDef = if (arraySize > 0) s"[$arraySize]" else ""
    val indent = if (hasChoice) INDENT else NO_INDENT
    val declaration = s"$indent    $definition $e$arrayDef;"

    structs.top.declarations += declaration

    // Add an array member to store a fixed length hexBinary element if needed
    if (
      child.isSimpleType && child.isFixedLength && child.optPrimType.get == PrimType.HexBinary
      && child.maybeFixedLengthInBits.get > 0
    ) {
      val fixedLength = child.maybeFixedLengthInBits.get / 8
      val declaration2 = s"$indent    uint8_t     _a_$e$arrayDef[$fixedLength];"
      structs.top.declarations += declaration2
    }
  }

  // Adds an element's ERD & offset to its parent element's children ERD & offset computations.
  private def addComputations(child: ElementBase): Unit = {
    val C = structs.top.C
    val e = cName(child)
    val hasArray = arrayMaxOccurs(child) > 0
    val arrayName = s"array_${cStructName(child)}$C"
    val erd = if (hasArray) s"${arrayName}ERD" else erdName(child)
    val deref = if (hasArray) "[0]" else ""
    val offsetComputation =
      s"    (const char *)&${C}_compute_offsets.$e$deref - (const char *)&${C}_compute_offsets"
    val erdComputation = s"    &$erd"
    structs.top.offsetComputations += offsetComputation
    structs.top.erdComputations += erdComputation
  }

  // Generates an array's ERD, childrenOffsets, childrenERDs, initERD, parseSelf, unparseSelf, getArraySize
  private def addArrayImplementation(elem: ElementBase): Unit = {
    val C = structs.top.C
    val e = cName(elem)
    val arrayName = s"array_${cStructName(elem)}$C"
    val erd = erdName(elem)
    val maxOccurs = elem.maxOccurs
    val minOccurs = elem.minOccurs
    val qNameInit = defineQNameInit(elem)

    // Add the array's ERD, childrenOffsets, childrenERDs
    val arrayERD =
      s"""static const $C ${arrayName}_compute_offsets;
         |
         |static const size_t ${arrayName}_childrenOffsets[1] = {
         |    (const char *)&${arrayName}_compute_offsets.$e[1] - (const char *)&${arrayName}_compute_offsets.$e[0]
         |};
         |
         |static const ERD *const ${arrayName}_childrenERDs[1] = {
         |    &$erd
         |};
         |
         |static const ERD ${arrayName}ERD = {
         |$qNameInit
         |    ARRAY, // typeCode
         |    $maxOccurs, // maxOccurs
         |    ${arrayName}_childrenOffsets,
         |    ${arrayName}_childrenERDs,
         |    (ERDParseSelf)&${arrayName}_parseSelf,
         |    (ERDUnparseSelf)&${arrayName}_unparseSelf,
         |    {.getArraySize = (GetArraySize)&${arrayName}_getArraySize}
         |};
         |""".stripMargin
    erds += arrayERD

    // Add the array's initERD, parseSelf, unparseSelf, getArraySize functions
    val initERDStatements =
      if (structs.top.initERDStatements.nonEmpty)
        s"""    UNUSED(parent);
         |    for (size_t i = 0; i < $maxOccurs; i++)
         |    {
         |${structs.top.initERDStatements.mkString("\n")}
         |    }""".stripMargin
      else
        s"""    UNUSED(instance);
         |    UNUSED(parent);""".stripMargin
    val parserStatements =
      s"""    const size_t arraySize = ${arrayName}_getArraySize(instance);
         |    validate_array_bounds("$arrayName", arraySize, $minOccurs, $maxOccurs, &pstate->pu);
         |    if (pstate->pu.error) return;
         |
         |    for (size_t i = 0; i < arraySize; i++)
         |    {
         |${structs.top.parserStatements.mkString("\n")}
         |    }""".stripMargin
    val unparserStatements =
      s"""    const size_t arraySize = ${arrayName}_getArraySize(instance);
         |    validate_array_bounds("$arrayName", arraySize, $minOccurs, $maxOccurs, &ustate->pu);
         |    if (ustate->pu.error) return;
         |
         |    for (size_t i = 0; i < arraySize; i++)
         |    {
         |${structs.top.unparserStatements.mkString("\n")}
         |    }""".stripMargin
    val arraySizeStatements = getOccursCount(elem)

    val prototypeFunctions =
      s"""static void ${arrayName}_parseSelf($C *instance, PState *pstate);
         |static void ${arrayName}_unparseSelf(const $C *instance, UState *ustate);
         |static size_t ${arrayName}_getArraySize(const $C *instance);""".stripMargin
    val functions =
      s"""static void
         |${arrayName}_initERD($C *instance, InfosetBase *parent)
         |{
         |$initERDStatements
         |}
         |
         |static void
         |${arrayName}_parseSelf($C *instance, PState *pstate)
         |{
         |$parserStatements
         |}
         |
         |static void
         |${arrayName}_unparseSelf(const $C *instance, UState *ustate)
         |{
         |$unparserStatements
         |}
         |
         |static size_t
         |${arrayName}_getArraySize(const $C *instance)
         |{
         |$arraySizeStatements
         |}
         |""".stripMargin

    prototypes += prototypeFunctions
    finalImplementation += functions
  }

  // Converts a choiceDispatchKey expression into a C struct dot notation
  // to access the C struct field containing the key's runtime value.
  private def choiceDispatchField(context: ElementBase): String = {
    // We handle only direct dispatch choices, so ignore other choices
    // and return "" for non-choice elements
    val dispatchField = context.complexType.modelGroup match {
      // Handle only direct dispatch choices
      case choice: Choice if choice.isDirectDispatch =>
        // Extract expression from {xs:string(...)} in element's choiceDispatchKey attribute
        val xml = choice.choiceDispatchKeyEv.expr.toBriefXML().filterNot(_.isWhitespace)
        val expr = xml.stripPrefix("'{xs:string(").stripSuffix(")}'")
        // Convert expression to a C struct field access
        val fieldAccess = cStructFieldAccess(expr)
        fieldAccess
      // Return "" for everything else
      case _ => ""
    }
    dispatchField
  }

  // Recursively builds a hopefully unique name using the given StringBuilder
  private def buildName(sc: SchemaComponent, sb: StringBuilder): StringBuilder = {
    // Append schema component's name
    sc match {
      case eb: ElementBase => sb ++= eb.namedQName.local += '_'
      case gd: GlobalElementDecl => sb ++= gd.namedQName.local += '_'
      case ct: GlobalComplexTypeDef => sb ++= ct.namedQName.local += '_'
      case _ => // don't include other schema components in qualified name
    }
    // Recursively append parent schema components' names
    sc.optLexicalParent.foreach {
      buildName(_, sb)
    }
    sb
  }

  // Makes any XML identifier a legal C identifier
  private def makeLegalForC(sb: StringBuilder): Unit = {
    // Remove the local name's namespace prefix if there is one; with
    // luck it won't be needed and the C code will look cleaner
    val matcher = Pattern.compile("^[^/:]+:").matcher(sb)
    if (matcher.lookingAt()) sb.replace(matcher.start, matcher.end, "")

    // Replace illegal characters with '_' to form a legal C name
    lazy val legalCharsForC: Set[Char] =
      Set('_') ++ ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
    for (i <- sb.indices) {
      if (!legalCharsForC.contains(sb.charAt(i))) {
        sb.setCharAt(i, '_')
      }
    }
  }

  // Generates the name part of an ERD definition
  private def defineQNameInit(context: ElementBase): String = {
    val prefix = context.namedQName.prefix.map(p => s""""$p"""").getOrElse("NULL")
    val local = context.namedQName.local // we want XML name not C name
    val nsUri = context.namedQName.namespace.toStringOrNullIfNoNS
    // Optimize away ns declaration if possible, although this approach may not be entirely correct
    val parentNsUri = context.enclosingElements.headOption
      .map(_.namedQName.namespace.toStringOrNullIfNoNS)
      .getOrElse("no-ns")
    val ns = if (nsUri == null || nsUri == parentNsUri) "NULL" else s""""$nsUri""""
    val qNameInit =
      s"""    {
         |        $prefix, // namedQName.prefix
         |        "$local", // namedQName.local
         |        $ns, // namedQName.ns
         |    },""".stripMargin
    qNameInit
  }

  // Gets the given element's primitive type while overriding type if needed to hold element's length
  private def getPrimType(e: ElementBase): PrimType = {
    val lengthInBits = getLengthInBits(e)
    val primType = e.optPrimType.get match {
      case PrimType.Boolean =>
        lengthInBits match {
          case n if n <= 32 => PrimType.Boolean
          case _ => e.SDE("Boolean lengths longer than 32 bits are not supported.")
        }
      case PrimType.Double | PrimType.Float =>
        lengthInBits match {
          case 32 => PrimType.Float
          case 64 => PrimType.Double
          case _ => e.SDE("Floating point lengths other than 32 or 64 bits are not supported.")
        }
      case PrimType.HexBinary => PrimType.HexBinary
      case PrimType.Byte | PrimType.Short | PrimType.Int | PrimType.Long | PrimType.Integer =>
        lengthInBits match {
          case n if n <= 8 => PrimType.Byte
          case n if n <= 16 => PrimType.Short
          case n if n <= 32 => PrimType.Int
          case n if n <= 64 => PrimType.Long
          case _ => e.SDE("Integer lengths longer than 64 bits are not supported.")
        }
      case PrimType.UnsignedByte | PrimType.UnsignedShort | PrimType.UnsignedInt |
          PrimType.UnsignedLong | PrimType.NonNegativeInteger =>
        lengthInBits match {
          case n if n <= 8 => PrimType.UnsignedByte
          case n if n <= 16 => PrimType.UnsignedShort
          case n if n <= 32 => PrimType.UnsignedInt
          case n if n <= 64 => PrimType.UnsignedLong
          case _ => e.SDE("Unsigned integer lengths longer than 64 bits are not supported.")
        }
      case p => e.SDE("PrimType %s is not supported in C code generator.", p.toString)
    }
    if (
      primType != e.optPrimType.get
      && e.optPrimType.get != PrimType.Integer
      && e.optPrimType.get != PrimType.NonNegativeInteger
    ) {
      e.SDW(
        WarnID.IgnoreDFDLProperty,
        "Ignoring PrimType %s, using %s",
        e.optPrimType.get.toString,
        primType.toString
      )
    }
    primType
  }

  // Returns the given element's maxOccurs if it is an array element
  // with a finite maxOccurs > 0, otherwise returns zero for scalar
  // elements and array elements with unbounded maxOccurs (we don't
  // support unbounded arrays in C right now)
  private def arrayMaxOccurs(e: ElementBase): Int = {
    val arrayMaxOccurs = e.occursCountKind match {
      case OccursCountKind.Fixed if e.maxOccurs > 1 => e.maxOccurs
      case OccursCountKind.Fixed if e.maxOccurs == 1 => 0
      case OccursCountKind.Implicit if e.minOccurs == 1 && e.maxOccurs == 1 => 0
      case OccursCountKind.Implicit if e.maxOccurs > 0 => e.maxOccurs
      case OccursCountKind.Expression if e.maxOccurs > 0 => e.maxOccurs
      case _ =>
        e.SDE(
          "occursCountKind %s minOccurs %d maxOccurs %d is not supported in C code generator",
          e.occursCountKind.toString,
          e.minOccurs,
          e.maxOccurs
        )
    }
    arrayMaxOccurs
  }

  // Returns the code needed to get the size of an array of elements, which
  // may be either a constant (maxOccurs) or an expression (occursCount)
  // which accesses a particular C struct field.
  private def getOccursCount(e: ElementBase): String = {
    val occursCount = e.occursCountKind match {
      case OccursCountKind.Fixed =>
        s"""    UNUSED(instance);
           |    return ${e.maxOccurs};""".stripMargin
      case OccursCountKind.Expression =>
        // Extract expression from {...} in element's occursCount attribute
        val expr = e.occursCountExpr.prettyExpr
          .filterNot(_.isWhitespace)
          .stripPrefix("{")
          .stripSuffix("}")
          .stripPrefix("xs:long(")
          .stripSuffix(")")
        // Convert expression to a C struct field access, stripping the first up
        // path (if any) because an occursCount's parent is the same struct while
        // a choiceDispatchKey's parent is an enclosing struct.
        val fieldAccess = cStructFieldAccess(expr.stripPrefix("../"))
        // Generate the rest of the code needed to access the field
        if (fieldAccess.startsWith("instance"))
          s"""    return $fieldAccess;"""
        else
          s"""    UNUSED(instance);
             |    return $fieldAccess;""".stripMargin
      case _ =>
        e.SDE(
          "getArraySize %s minOccurs %d maxOccurs %d is not supported in C code generator",
          e.occursCountKind.toString,
          e.minOccurs,
          e.maxOccurs
        )
    }
    occursCount
  }

  // Returns the notation needed to access a C struct field.  We make some simplifying
  // assumptions to make generating the field access easier:
  // - the expression contains only a relative or absolute path, nothing else (e.g.,
  //   the expression doesn't call any functions or perform any computation)
  // - we can convert an absolute path to a get_infoset()-> indirection
  // - we can convert a relative path beginning with up dirs to a parents-> indirection
  // - we can convert a relative path without any up dirs to an instance-> indirection
  // - we can convert slashes in the path to dots in a C struct field access notation
  private def cStructFieldAccess(expr: String): String = {
    // Turn all DFDL local names into legal C names
    val localName = """([\p{L}_][\p{L}:_\-.0-9]*)""".r
    val exprWithFields = localName.replaceAllIn(
      expr,
      m => {
        // Make each DFDL local name a legal C name
        val sb = new StringBuilder(m.group(1))
        makeLegalForC(sb)
        sb.mkString
      }
    )

    // Convert exprPath to the appropriate field access indirection
    val fieldAccess = if (exprWithFields.startsWith("/")) {
      // Strip the root element's name from exprWithFields
      val rootName = root.namedQName.local
      val exprWORoot = exprWithFields.stripPrefix(s"/$rootName/")
      // Convert exprWORoot to a get_infoset()-> indirection
      val C = cStructName(root)
      s"""(($C *)get_infoset(false))->$exprWORoot"""
    } else if (exprWithFields.startsWith("../")) {
      // Split exprPath into the up dirs and after the up dirs
      val afterUpDirs = exprWithFields.split("\\.\\./").mkString
      val upDirs = exprWithFields.stripSuffix(afterUpDirs)
      // Count how many up dirs there are
      val nUpDirs = upDirs.split('/').length
      // Go up the stack that many times to get that struct's C type
      val C = structs(nUpDirs).C
      // Convert the up dirs to parents
      val parents = upDirs.replace("../", "parent->").stripSuffix("->")
      // Convert exprPath to a parents-> indirection
      s"""(($C *)instance->_base.$parents)->$afterUpDirs"""
    } else {
      // Convert exprPath to an instance-> indirection
      s"""instance->$exprWithFields"""
    }

    // Finally, convert the field access to C struct dot notation
    val notation = fieldAccess.replace('/', '.')
    notation
  }

  // Gets length from explicit length declaration if any, otherwise from base type's implicit length
  private def getLengthInBits(e: ElementBase): Long = {
    // Skip HexBinary elements since some of them won't have a constant length
    if (e.optPrimType.get == PrimType.HexBinary)
      0
    else {
      e.schemaDefinitionUnless(
        e.elementLengthInBitsEv.isConstant,
        "Runtime dfdl:length expressions are not supported."
      )
      e.elementLengthInBitsEv.constValue.get
    }
  }
}

/**
 * Accumulates strings of generated C code for nested elements inside
 * complex elements.
 */
class ComplexCGState(
  val C: String,
  val context: ElementBase = null,
  val inArray: Boolean = false
) {
  val declarations: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val offsetComputations: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val erdComputations: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val initERDStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val initChoiceStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val parserStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val unparserStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
}
