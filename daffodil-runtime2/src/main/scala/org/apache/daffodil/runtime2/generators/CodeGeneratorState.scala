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

import org.apache.daffodil.api.WarnID
import org.apache.daffodil.cookers.ChoiceBranchKeyCooker
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dsom.AbstractElementRef
import org.apache.daffodil.dsom.Choice
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.dsom.GlobalComplexTypeDef
import org.apache.daffodil.dsom.GlobalElementDecl
import org.apache.daffodil.dsom.SchemaComponent
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind

import java.net.URI
import scala.collection.mutable

/**
 * Builds up the state of generated code.
 */
class CodeGeneratorState {
  private val structs = mutable.Stack[ComplexCGState]()
  private val prototypes = mutable.ArrayBuffer[String]()
  private val erds = mutable.ArrayBuffer[String]()
  private val finalStructs = mutable.ArrayBuffer[String]()
  private val finalImplementation = mutable.ArrayBuffer[String]()

  // Builds an ERD name for the given element that needs to be unique in C file scope
  private def erdName(context: ElementBase): String = {
    def buildName(sc: SchemaComponent, sb: StringBuilder): StringBuilder = {
      sc match {
        case eb: ElementBase => sb ++= eb.namedQName.local += '_'
        case gd: GlobalElementDecl => sb ++= gd.namedQName.local += '_'
        case ct: GlobalComplexTypeDef => sb ++= ct.namedQName.local += '_'
        case _ => // don't include other schema components in qualified name
      }
      sc.optLexicalParent.foreach {
        buildName(_, sb)
      }
      sb
    }
    val sb = buildName(context, new StringBuilder) ++= "ERD"
    sb.toString()
  }

  // Returns the given element's local name (doesn't have to be unique)
  private def localName(context: ElementBase): String = context.namedQName.local

  def addImplementation(context: ElementBase): Unit = {
    val C = localName(context)
    val initStatements = structs.top.initStatements.mkString("\n")
    val initChoiceStatements = structs.top.initChoiceStatements.mkString("\n")
    val hasStatements = structs.top.parserStatements.nonEmpty
    val parserStatements = if (hasStatements)
      structs.top.parserStatements.mkString("\n")
    else
      s"""    // Empty struct, but need to prevent compiler warnings
         |    UNUSED(${C}_compute_offsets);
         |    UNUSED(instance);
         |    UNUSED(pstate);""".stripMargin
    val unparserStatements = if (hasStatements)
      structs.top.unparserStatements.mkString("\n")
    else
      s"""    // Empty struct, but need to prevent compiler warnings
         |    UNUSED(${C}_compute_offsets);
         |    UNUSED(instance);
         |    UNUSED(ustate);""".stripMargin
    val hasChoice = structs.top.initChoiceStatements.nonEmpty
    val root = structs.elems.last.C
    val prototypeInitChoice = if (hasChoice)
      s"\nstatic bool ${C}_initChoice($C *instance, const $root *rootElement);"
    else
      ""
    val implementInitChoice = if (hasChoice)
      s"""
         |static bool
         |${C}_initChoice($C *instance, const $root *rootElement)
         |{
         |$initChoiceStatements
         |}
         |""".stripMargin
    else
      ""
    val prototypeFunctions =
      s"""static void ${C}_initSelf($C *instance);$prototypeInitChoice
         |static void ${C}_parseSelf($C *instance, PState *pstate);
         |static void ${C}_unparseSelf(const $C *instance, UState *ustate);""".stripMargin
    val functions =
      s"""static void
         |${C}_initSelf($C *instance)
         |{
         |$initStatements
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

  private def defineQNameInit(context: ElementBase): String = {
    val prefix = context.namedQName.prefix.map(p => s""""$p"""").getOrElse("NULL")
    val local = localName(context)
    val nsUri = context.namedQName.namespace.toStringOrNullIfNoNS
    // Optimize away ns declaration if possible, although this approach may not be entirely correct
    val parentNsUri = context.enclosingElements.headOption.map(_.namedQName.namespace.toStringOrNullIfNoNS).getOrElse("no-ns")
    val ns = if (nsUri == null || nsUri == parentNsUri) "NULL" else s""""$nsUri""""
    val qnameInit =
      s"""    {
         |        $prefix, // namedQName.prefix
         |        "$local", // namedQName.local
         |        $ns, // namedQName.ns
         |    },""".stripMargin
    qnameInit
  }

  /**
   * We want to convert a choiceDispatchKey expression into C struct dot
   * notation (rootElement->[subElement.field]) which will access the C
   * struct field containing the choiceDispatchKey's runtime value.
   *
   * We make some assumptions to make generating the dot notation easier:
   * - the expression starts with '{xs:string( and ends with )}'
   * - the expression returns the value of a previous element without
   *   changing the value in any way (except converting it to xs:string)
   * - both the expression and the C code use only local names (for now...)
   * - we can map the context node's path to a Unix-like slash path
   * - all dpath operations look like Unix-like relative paths (../tag)
   * - we can normalize the new path and convert it to C struct dot notation
   * - we can store the accessed value in an int64_t local variable safely
   */
  private def choiceDispatchField(context: ElementBase): String = {
    // We want to use SchemaComponent.scPath but it's private so duplicate it here (for now...)
    def scPath(sc: SchemaComponent): Seq[SchemaComponent] = sc.optLexicalParent.map { scPath }.getOrElse(Nil) :+ sc
    val localNames = scPath(context).map {
      case er: AbstractElementRef => er.refQName.local
      case e: ElementBase => e.namedQName.local
      case ed: GlobalElementDecl => ed.namedQName.local
      case _ => ""
    }
    val absoluteSlashPath = localNames.mkString("/")
    val dispatchSlashPath = context.complexType.modelGroup match {
      case choice: Choice if choice.isDirectDispatch =>
        val expr = choice.choiceDispatchKeyEv.expr.toBriefXML()
        val before = "'{xs:string("
        val after = ")}'"
        val relativePath = if (expr.startsWith(before) && expr.endsWith(after))
          expr.substring(before.length, expr.length - after.length) else expr
        val normalizedURI = new URI(absoluteSlashPath + "/" + relativePath).normalize
        normalizedURI.getPath.substring(1)
      case _ => ""
    }
    // Strip namespace prefixes since C code uses only local names (for now...)
    val localDispatchSlashPath = dispatchSlashPath.replaceAll("/[^:]+:", "/")
    val res = localDispatchSlashPath.replace('/', '.')
    res
  }

  // We know context is a complex type.  We need to 1) support choice groups; 2) support
  // padding complex elements to explicit lengths with fill bytes
  def addBeforeSwitchStatements(context: ElementBase): Unit = {
    val erd = erdName(context)
    val initStatement = s"    instance->_base.erd = &$erd;"

    structs.top.initStatements += initStatement

    // Implement padding if complex type has an explicit length
    if (context.maybeFixedLengthInBits.isDefined && context.maybeFixedLengthInBits.get > 0) {
      val lengthInBytes = context.maybeFixedLengthInBits.get / 8;
      val parseStatement = s"    const size_t end_position = pstate->position + $lengthInBytes;"
      val unparseStatement = s"    const size_t end_position = ustate->position + $lengthInBytes;"

      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }

    val dispatchField = choiceDispatchField(context)
    if (dispatchField.nonEmpty) {
      val C = localName(context)
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
           |    0, NULL, NULL, NULL, NULL, NULL, NULL
           |};
           |""".stripMargin
      val offsetComputation = s"    (const char *)&${C}_compute_offsets._choice - (const char *)&${C}_compute_offsets"
      val erdComputation = s"    &_choice_$erd"
      val initStatement = s"    instance->_choice = NO_CHOICE;"
      val initChoiceStatement =
        s"""    int64_t key = rootElement->$dispatchField;
           |    switch (key)
           |    {""".stripMargin
      val parseStatement =
        s"""    instance->_base.erd->initChoice(&instance->_base, rootElement());
           |    switch (instance->_choice)
           |    {""".stripMargin
      val unparseStatement =
        s"""    instance->_base.erd->initChoice(&instance->_base, rootElement());
           |    switch (instance->_choice)
           |    {""".stripMargin

      erds += erdDef
      structs.top.declarations += declaration
      structs.top.offsetComputations += offsetComputation
      structs.top.erdComputations += erdComputation
      structs.top.initStatements += initStatement
      structs.top.initChoiceStatements += initChoiceStatement
      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }
  }

  // We know context is a complex type.  We need to 1) support choice groups; 2) support
  // padding complex elements to explicit lengths with fill bytes
  def addAfterSwitchStatements(context: ElementBase): Unit = {
    if (structs.top.initChoiceStatements.nonEmpty) {
      val declaration = s"    };"
      val initChoiceStatement =
        s"""    default:
           |        instance->_choice = NO_CHOICE;
           |        break;
           |    }
           |
           |    if (instance->_choice != NO_CHOICE)
           |    {
           |        const size_t choice = instance->_choice + 1; // skip the _choice field
           |        const size_t offset = instance->_base.erd->offsets[choice];
           |        const ERD *  childERD = instance->_base.erd->childrenERDs[choice];
           |        InfosetBase *childNode = (InfosetBase *)((const char *)instance + offset);
           |        childNode->erd = childERD;
           |        return true;
           |    }
           |    else
           |    {
           |        return false;
           |    }""".stripMargin
      val parseStatement =
        s"""    default:
           |        pstate->error_msg =
           |            "Parse error: no match between choice dispatch key and any branch key";
           |        break;
           |    }""".stripMargin
      val unparseStatement =
        s"""    default:
           |        ustate->error_msg =
           |            "Unparse error: no match between choice dispatch key and any branch key";
           |        break;
           |    }""".stripMargin

      structs.top.declarations += declaration
      structs.top.initChoiceStatements += initChoiceStatement
      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }

    // Implement padding if complex type has an explicit length
    if (context.maybeFixedLengthInBits.isDefined && context.maybeFixedLengthInBits.get > 0) {
      val octalFillByte = context.fillByteEv.constValue.toByte.toOctalString
      val parseStatement = s"    parse_fill_bytes(end_position, pstate);"
      val unparseStatement = s"    unparse_fill_bytes(end_position, '\\$octalFillByte', ustate);"

      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }
  }

  def addComplexTypeERD(context: ElementBase): Unit = {
    val C = localName(context)
    val erd = erdName(context)
    val count = structs.top.offsetComputations.length
    val offsetComputations = structs.top.offsetComputations.mkString(",\n")
    val erdComputations = structs.top.erdComputations.mkString(",\n")
    val qnameInit = defineQNameInit(context)
    val hasChoice = structs.top.initChoiceStatements.nonEmpty
    val numChildren = if (hasChoice) 2 else count
    val initChoice = if (hasChoice) s"(InitChoiceRD)&${C}_initChoice" else "NULL"
    val complexERD =
      s"""static const $C ${C}_compute_offsets;
         |
         |static const size_t ${C}_offsets[$count] = {
         |$offsetComputations
         |};
         |
         |static const ERD *${C}_childrenERDs[$count] = {
         |$erdComputations
         |};
         |
         |static const ERD $erd = {
         |$qnameInit
         |    COMPLEX, // typeCode
         |    $numChildren, // numChildren
         |    ${C}_offsets, // offsets
         |    ${C}_childrenERDs, // childrenERDs
         |    (ERDInitSelf)&${C}_initSelf, // initSelf
         |    (ERDParseSelf)&${C}_parseSelf, // parseSelf
         |    (ERDUnparseSelf)&${C}_unparseSelf, // unparseSelf
         |    $initChoice // initChoice
         |};
         |""".stripMargin

    erds += complexERD
  }

  def addStruct(context: ElementBase): Unit = {
    val C = localName(context)
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

  def addSimpleTypeStatements(initStatement: String, parseStatement: String, unparseStatement: String): Unit = {
    if (initStatement.nonEmpty) structs.top.initStatements += initStatement
    if (parseStatement.nonEmpty) structs.top.parserStatements += parseStatement
    if (unparseStatement.nonEmpty) structs.top.unparserStatements += unparseStatement
  }

  def addComplexTypeStatements(child: ElementBase): Unit = {
    val C = localName(child)
    val e = child.name
    val hasChoice = structs.top.initChoiceStatements.nonEmpty
    val arraySize = if (child.occursCountKind == OccursCountKind.Fixed) child.maxOccurs else 0

    if (hasChoice) {
      val offset = child.position - 1
      val initChoiceStatement =
        s"""        instance->_choice = $offset;
           |        break;""".stripMargin
      val parseStatement = s"    case $offset:"
      val unparseStatement = s"    case $offset:"

      structs.top.initChoiceStatements ++= ChoiceBranchKeyCooker.convertConstant(
        child.choiceBranchKey, child, forUnparse = false).map { key => s"    case $key:"}
      structs.top.initChoiceStatements += initChoiceStatement
      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }

    def addStatements(deref: String): Unit = {
      val moreIndent = if (hasChoice) "    " else ""
      val initStatement = s"    ${C}_initSelf(&instance->$e$deref);"
      val parseStatement = s"    $moreIndent${C}_parseSelf(&instance->$e$deref, pstate);"
      val unparseStatement = s"    $moreIndent${C}_unparseSelf(&instance->$e$deref, ustate);"

      structs.top.initStatements += initStatement
      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }
    if (arraySize > 0)
      for (i <- 0 until arraySize)
        addStatements(s"[$i]")
    else
      addStatements("")

    if (hasChoice) {
      val parseStatement = s"        break;"
      val unparseStatement = s"        break;"

      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }
  }

  def pushComplexElement(context: ElementBase): Unit = {
    val C = localName(context)
    structs.push(new ComplexCGState(C))
  }

  def popComplexElement(): Unit = {
    structs.pop()
  }

  // Gets length from explicit length declaration if any, otherwise from base type's implicit length
  private def getLengthInBits(e: ElementBase): Long = {
    e.schemaDefinitionUnless(e.elementLengthInBitsEv.isConstant, "Runtime dfdl:length expressions are not supported.")
    e.elementLengthInBitsEv.constValue.get
  }

  // Because schema authors don't always get types right, allows explicit lengths to override implicit lengths
  private def getPrimType(e: ElementBase): PrimType = {
    val primType = e.optPrimType.get match {
      case PrimType.Byte
         | PrimType.Short
         | PrimType.Int
         | PrimType.Long
         | PrimType.Integer =>
        getLengthInBits(e) match {
          case 8 =>  PrimType.Byte
          case 16 => PrimType.Short
          case 32 => PrimType.Int
          case 64 => PrimType.Long
          case _ =>  e.SDE("Integer lengths other than 8, 16, 32, or 64 bits are not supported.")
        }
      case PrimType.UnsignedByte
         | PrimType.UnsignedShort
         | PrimType.UnsignedInt
         | PrimType.UnsignedLong
         | PrimType.NonNegativeInteger =>
        getLengthInBits(e) match {
          case 8 =>  PrimType.UnsignedByte
          case 16 => PrimType.UnsignedShort
          case 32 => PrimType.UnsignedInt
          case 64 => PrimType.UnsignedLong
          case _ =>  e.SDE("Unsigned integer lengths other than 8, 16, 32, or 64 bits are not supported.")
        }
      case PrimType.Double
         | PrimType.Float =>
        getLengthInBits(e) match {
          case 32 => PrimType.Float
          case 64 => PrimType.Double
          case _ =>  e.SDE("Floating point lengths other than 32 or 64 bits are not supported.")
        }
      case PrimType.Boolean =>
        getLengthInBits(e) match {
          case 8 | 16 | 32 => PrimType.Boolean
          case _ => e.SDE("Boolean lengths other than 8, 16, or 32 bits are not supported.")
        }
      case p => e.SDE("PrimType %s is not supported in C code generator.", p.toString)
    }
    if (primType != e.optPrimType.get)
      e.SDW(WarnID.IgnoreDFDLProperty, "Ignoring PrimType %s, using %s", e.optPrimType.get.toString, primType.toString)
    primType
  }

  def addSimpleTypeERD(context: ElementBase): Unit = {
    val erd = erdName(context)
    val qnameInit = defineQNameInit(context)
    val typeCode = getPrimType(context) match {
      case PrimType.Boolean => "PRIMITIVE_BOOLEAN"
      case PrimType.Double => "PRIMITIVE_DOUBLE"
      case PrimType.Float => "PRIMITIVE_FLOAT"
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
    val erdDef =
      s"""static const ERD $erd = {
         |$qnameInit
         |    $typeCode, // typeCode
         |    0, NULL, NULL, NULL, NULL, NULL, NULL
         |};
         |""".stripMargin
    erds += erdDef
    addComputations(context)
  }

  def addComputations(child: ElementBase): Unit = {
    val C = structs.top.C
    val e = localName(child)
    val erd = erdName(child)
    val arraySize = if (child.occursCountKind == OccursCountKind.Fixed) child.maxOccurs else 0
    def addComputation(deref: String): Unit = {
      val offsetComputation = s"    (const char *)&${C}_compute_offsets.$e$deref - (const char *)&${C}_compute_offsets"
      val erdComputation = s"    &$erd"
      structs.top.offsetComputations += offsetComputation
      structs.top.erdComputations += erdComputation
    }
    if (arraySize > 0)
      for (i <- 0 until arraySize)
        addComputation(s"[$i]")
    else
      addComputation("")
  }

  def addFieldDeclaration(context: ThrowsSDE, child: ElementBase): Unit = {
    val definition = if (child.isSimpleType) {
      getPrimType(child) match {
        case PrimType.Boolean => "bool       "
        case PrimType.Double => "double     "
        case PrimType.Float => "float      "
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
      localName(child)
    }
    val e = child.name
    val arrayDef = if (child.occursCountKind == OccursCountKind.Fixed) s"[${child.maxOccurs}]" else ""
    val indent = if (structs.top.initChoiceStatements.nonEmpty) "    " else ""
    val declaration = s"$indent    $definition $e$arrayDef;"

    structs.top.declarations += declaration
  }

  def generateCodeHeader: String = {
    val structs = finalStructs.mkString("\n")
    val header =
      s"""#ifndef GENERATED_CODE_H
         |#define GENERATED_CODE_H
         |
         |#include "infoset.h"  // for InfosetBase
         |#include <stdbool.h>  // for bool
         |#include <stdint.h>   // for int16_t, int32_t, int64_t, int8_t, uint16_t, uint32_t, uint64_t, uint8_t

         |// Define infoset structures
         |
         |$structs
         |#endif // GENERATED_CODE_H
         |""".stripMargin
    header
  }

  def generateCodeFile(rootElementName: String): String = {
    val prototypes = this.prototypes.mkString("\n")
    val erds = this.erds.mkString("\n")
    val finalImplementation = this.finalImplementation.mkString("\n")
    val code =
      s"""#include "generated_code.h"
         |#include "parsers.h"    // for parse_be_double, parse_be_float, parse_be_int16, parse_be_int32, parse_be_int64, parse_be_int8, parse_be_uint16, parse_be_uint32, parse_be_uint64, parse_be_uint8, parse_le_double, parse_le_float, parse_le_int16, parse_le_int32, parse_le_int64, parse_le_int8, parse_le_uint16, parse_le_uint32, parse_le_uint64, parse_le_uint8
         |#include "unparsers.h"  // for unparse_be_double, unparse_be_float, unparse_be_int16, unparse_be_int32, unparse_be_int64, unparse_be_int8, unparse_be_uint16, unparse_be_uint32, unparse_be_uint64, unparse_be_uint8, unparse_le_double, unparse_le_float, unparse_le_int16, unparse_le_int32, unparse_le_int64, unparse_le_int8, unparse_le_uint16, unparse_le_uint32, unparse_le_uint64, unparse_le_uint8
         |#include <math.h>       // for NAN
         |#include <stdbool.h>    // for bool, false, true
         |#include <stddef.h>     // for NULL, size_t
         |
         |// Prototypes needed for compilation
         |
         |$prototypes
         |
         |// Metadata singletons
         |
         |$erds
         |// Return a root element to be used for parsing or unparsing
         |
         |InfosetBase *
         |rootElement(void)
         |{
         |    static bool initialized;
         |    static $rootElementName root;
         |    if (!initialized)
         |    {
         |        ${rootElementName}_initSelf(&root);
         |        initialized = true;
         |    }
         |    return &root._base;
         |}
         |
         |// Methods to initialize, parse, and unparse infoset nodes
         |
         |$finalImplementation
         |""".stripMargin
    code
  }
}

/**
 * Accumulates strings of generated C code for nested elements inside
 * complex elements.
 */
class ComplexCGState(val C: String) {
  val declarations = mutable.ArrayBuffer[String]()
  val offsetComputations = mutable.ArrayBuffer[String]()
  val erdComputations = mutable.ArrayBuffer[String]()
  val initStatements = mutable.ArrayBuffer[String]()
  val initChoiceStatements = mutable.ArrayBuffer[String]()
  val parserStatements = mutable.ArrayBuffer[String]()
  val unparserStatements = mutable.ArrayBuffer[String]()
}
