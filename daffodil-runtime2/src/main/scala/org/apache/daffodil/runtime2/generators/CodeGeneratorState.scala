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
import org.apache.daffodil.dsom.Root
import org.apache.daffodil.dsom.SchemaComponent
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind

import java.net.URI
import scala.collection.mutable

/**
 * Builds up the state of generated code.
 */
class CodeGeneratorState(private val root: ElementBase) {
  private val elementsAlreadySeen = mutable.Map[String, ElementBase]()
  private val structs = mutable.Stack[ComplexCGState]()
  private val prototypes = mutable.ArrayBuffer[String]()
  private val erds = mutable.ArrayBuffer[String]()
  private val finalStructs = mutable.ArrayBuffer[String]()
  private val finalImplementation = mutable.ArrayBuffer[String]()

  // Recursively builds a hopefully unique name using the given StringBuilder
  private def buildName(sc: SchemaComponent, sb: StringBuilder): StringBuilder = {
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

  // Returns a name for the given element's C struct identifier
  private def cStructName(context: ElementBase): String = {
    val sb = buildName(context, new StringBuilder)
    val name = sb.toString
    name
  }

  // Returns a name for the given element's element resource descriptor
  private def erdName(context: ElementBase): String = {
    val sb = buildName(context, new StringBuilder) ++= "ERD"
    val name = sb.toString
    name
  }

  // Returns true if the element has not been seen before (checking if a
  // map already contains the element, otherwise adding it to the map)
  def elementNotSeenYet(context: ElementBase): Boolean = {
    val key = cStructName(context)
    val alreadySeen = elementsAlreadySeen.contains(key)
    if (!alreadySeen)
      elementsAlreadySeen += (key -> context)
    !alreadySeen
  }

  def addImplementation(context: ElementBase): Unit = {
    val C = cStructName(context)
    val initERDStatements = structs.top.initERDStatements.mkString("\n")
    val initSelfStatements = if (structs.top.initSelfStatements.nonEmpty)
      structs.top.initSelfStatements.mkString("\n")
    else
      s"""    // Empty struct, but need to prevent compiler warnings
         |    UNUSED(instance);""".stripMargin
    val initChoiceStatements = structs.top.initChoiceStatements.mkString("\n")
    val parserStatements = if (structs.top.parserStatements.nonEmpty)
      structs.top.parserStatements.mkString("\n")
    else
      s"""    // Empty struct, but need to prevent compiler warnings
         |    UNUSED(instance);
         |    UNUSED(pstate);""".stripMargin
    val unparserStatements = if (structs.top.unparserStatements.nonEmpty)
      structs.top.unparserStatements.mkString("\n")
    else
      s"""    // Empty struct, but need to prevent compiler warnings
         |    UNUSED(instance);
         |    UNUSED(ustate);""".stripMargin
    val hasChoice = structs.top.initChoiceStatements.nonEmpty
    val rootName = cStructName(root)
    val prototypeInitChoice = if (hasChoice)
      s"static const Error *${C}_initChoice($C *instance, const $rootName *rootElement);\n"
    else
      ""
    val implementInitChoice = if (hasChoice)
      s"""
         |static const Error *
         |${C}_initChoice($C *instance, const $rootName *rootElement)
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
         |${C}_initERD($C *instance)
         |{
         |$initERDStatements
         |}
         |
         |static void
         |${C}_initSelf($C *instance)
         |{
         |$initSelfStatements
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
    val local = context.namedQName.local
    val nsUri = context.namedQName.namespace.toStringOrNullIfNoNS
    // Optimize away ns declaration if possible, although this approach may not be entirely correct
    val parentNsUri = context.enclosingElements.headOption.map(_.namedQName.namespace.toStringOrNullIfNoNS).getOrElse("no-ns")
    val ns = if (nsUri == null || nsUri == parentNsUri) "NULL" else s""""$nsUri""""
    val qNameInit =
      s"""    {
         |        $prefix, // namedQName.prefix
         |        "$local", // namedQName.local
         |        $ns, // namedQName.ns
         |    },""".stripMargin
    qNameInit
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
    // We want to call SchemaComponent.scPath but it's private so duplicate it here for now
    def scPath(sc: SchemaComponent): Seq[SchemaComponent] = sc.optLexicalParent.map { scPath }.getOrElse(Nil) :+ sc

    // We handle only direct dispatch choices, so ignore other elements
    context.complexType.modelGroup match {
      case choice: Choice if choice.isDirectDispatch =>
        // Get parent path against which to perform up paths
        val parentNames = scPath(context).map {
          case _: Root => ""
          case er: AbstractElementRef => er.refQName.local
          case eb: ElementBase => eb.namedQName.local
          case ed: GlobalElementDecl => ed.namedQName.local
          case _ => ""
        }
        val parentPath = parentNames.mkString("/")

        // Convert expression to a relative path (may have up paths)
        val expr = choice.choiceDispatchKeyEv.expr.toBriefXML().filterNot(_.isWhitespace)
        val before = "'{xs:string("
        val after = ")}'"
        val relativePath = if (expr.startsWith(before) && expr.endsWith(after))
          expr.substring(before.length, expr.length - after.length) else expr

        // Remove redundant slashes (//) and up paths (../)
        val normalizedURI = new URI(parentPath + "/" + relativePath).normalize

        // Strip namespace prefixes since C code uses only local names (for now)
        val dispatchPath = normalizedURI.getPath.replaceAll("/[^/:]+:", "/")

        // Convert to C struct dot notation without any leading dot
        val notation = dispatchPath.replace('/', '.').substring(1)
        notation
      // We get called on every group element, so we need to return "" for non-choice elements
      case _ => ""
    }
  }

  // We know context is a complex type.  We need to 1) support choice groups; 2) support
  // padding complex elements to explicit lengths with fill bytes
  def addBeforeSwitchStatements(context: ElementBase): Unit = {
    val erd = erdName(context)
    val initERDStatement = s"    instance->_base.erd = &$erd;"

    structs.top.initERDStatements += initERDStatement

    // Implement padding if complex type has an explicit length
    if (context.maybeFixedLengthInBits.isDefined && context.maybeFixedLengthInBits.get > 0) {
      val lengthInBits = context.maybeFixedLengthInBits.get
      val parseStatement = s"    const size_t end_bitPos0b = pstate->bitPos0b + $lengthInBits;"
      val unparseStatement = s"    const size_t end_bitPos0b = ustate->bitPos0b + $lengthInBits;"

      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }

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
           |    0, NULL, NULL, NULL, NULL, NULL
           |};
           |""".stripMargin
      val offsetComputation = s"    (const char *)&${C}_compute_offsets._choice - (const char *)&${C}_compute_offsets"
      val erdComputation = s"    &_choice_$erd"
      val initSelfStatement = s"    instance->_choice = 0xFFFFFFFFFFFFFFFF;"
      val initChoiceStatement =
        s"""    static Error error = {ERR_CHOICE_KEY, {0}};
           |
           |    int64_t key = rootElement->$dispatchField;
           |    switch (key)
           |    {""".stripMargin
      val parseStatement =
        s"""    static Error error = {ERR_CHOICE_KEY, {0}};
           |
           |    pstate->error = instance->_base.erd->initChoice(&instance->_base, rootElement());
           |    if (pstate->error) return;
           |
           |    switch (instance->_choice)
           |    {""".stripMargin
      val unparseStatement =
        s"""    static Error error = {ERR_CHOICE_KEY, {0}};
           |
           |    ustate->error = instance->_base.erd->initChoice(&instance->_base, rootElement());
           |    if (ustate->error) return;
           |
           |    switch (instance->_choice)
           |    {""".stripMargin

      erds += erdDef
      structs.top.declarations += declaration
      structs.top.offsetComputations += offsetComputation
      structs.top.erdComputations += erdComputation
      structs.top.initSelfStatements += initSelfStatement
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
           |        error.arg.d64 = key;
           |        return &error;
           |    }
           |
           |    return NULL;""".stripMargin
      val parseStatement =
        s"""    default:
           |        // Should never happen because initChoice would return an error first
           |        error.arg.d64 = (int64_t)instance->_choice;
           |        pstate->error = &error;
           |        return;
           |    }""".stripMargin
      val unparseStatement =
        s"""    default:
           |        // Should never happen because initChoice would return an error first
           |        error.arg.d64 = (int64_t)instance->_choice;
           |        ustate->error = &error;
           |        return;
           |    }""".stripMargin

      structs.top.declarations += declaration
      structs.top.initChoiceStatements += initChoiceStatement
      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }

    // Implement padding if complex type has an explicit length
    if (context.maybeFixedLengthInBits.isDefined && context.maybeFixedLengthInBits.get > 0) {
      val octalFillByte = context.fillByteEv.constValue.toByte.toOctalString
      val parseStatement =
        s"""    parse_fill_bits(end_bitPos0b, pstate);
           |    if (pstate->error) return;""".stripMargin
      val unparseStatement =
        s"""    unparse_fill_bits(end_bitPos0b, '\\$octalFillByte', ustate);
           |    if (ustate->error) return;""".stripMargin

      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }
  }

  def addComplexTypeERD(context: ElementBase): Unit = {
    val C = cStructName(context)
    val erd = erdName(context)
    val count = structs.top.offsetComputations.length
    val offsetComputations = structs.top.offsetComputations.mkString(",\n")
    val erdComputations = structs.top.erdComputations.mkString(",\n")
    val qNameInit = defineQNameInit(context)
    val hasChoice = structs.top.initChoiceStatements.nonEmpty
    val numChildren = if (hasChoice) 2 else count
    val initChoice = if (hasChoice) s"(InitChoiceRD)&${C}_initChoice" else "NULL"
    val complexERD = if (numChildren > 0)
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
         |$qNameInit
         |    COMPLEX, // typeCode
         |    $numChildren, // numChildren
         |    ${C}_offsets, // offsets
         |    ${C}_childrenERDs, // childrenERDs
         |    (ERDParseSelf)&${C}_parseSelf, // parseSelf
         |    (ERDUnparseSelf)&${C}_unparseSelf, // unparseSelf
         |    $initChoice // initChoice
         |};
         |""".stripMargin
    else
      s"""static const ERD $erd = {
         |$qNameInit
         |    COMPLEX, // typeCode
         |    $numChildren, // numChildren
         |    NULL, // offsets
         |    NULL, // childrenERDs
         |    (ERDParseSelf)&${C}_parseSelf, // parseSelf
         |    (ERDUnparseSelf)&${C}_unparseSelf, // unparseSelf
         |    $initChoice // initChoice
         |};
         |""".stripMargin

    erds += complexERD
  }

  def addStruct(context: ElementBase): Unit = {
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

  def addSimpleTypeStatements(initERDStatement: String, initSelfStatement: String, parseStatement: String, unparseStatement: String): Unit = {
    if (initERDStatement.nonEmpty) structs.top.initERDStatements += initERDStatement
    if (initSelfStatement.nonEmpty) structs.top.initSelfStatements += initSelfStatement
    if (parseStatement.nonEmpty) structs.top.parserStatements += parseStatement
    if (unparseStatement.nonEmpty) structs.top.unparserStatements += unparseStatement
  }

  def addComplexTypeStatements(child: ElementBase): Unit = {
    val C = cStructName(child)
    val e = child.name
    val hasChoice = structs.top.initChoiceStatements.nonEmpty
    val arraySize = if (child.occursCountKind == OccursCountKind.Fixed) child.maxOccurs else 0

    if (hasChoice) {
      structs.top.initChoiceStatements ++= ChoiceBranchKeyCooker.convertConstant(
        child.choiceBranchKey, child, forUnparse = false).map { key => s"    case $key:"}

      val offset = child.position - 1
      val initChoiceStatement = s"        instance->_choice = $offset;"
      val parseStatement = s"    case $offset:"
      val unparseStatement = s"    case $offset:"

      structs.top.initChoiceStatements += initChoiceStatement
      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }

    def addStatements(deref: String): Unit = {
      val initChoiceStatement = s"        ${C}_initERD(&instance->$e$deref);"
      val initERDStatement = s"    ${C}_initERD(&instance->$e$deref);"
      val initSelfStatement = s"    ${C}_initSelf(&instance->$e$deref);"
      val moreIndent = if (hasChoice) "    " else ""
      val parseStatement =
        s"""$moreIndent    ${C}_parseSelf(&instance->$e$deref, pstate);
           |$moreIndent    if (pstate->error) return;""".stripMargin
      val unparseStatement =
        s"""$moreIndent    ${C}_unparseSelf(&instance->$e$deref, ustate);
           |$moreIndent    if (ustate->error) return;""".stripMargin

      if (hasChoice)
        structs.top.initChoiceStatements += initChoiceStatement
      else
        structs.top.initERDStatements += initERDStatement
      structs.top.initSelfStatements += initSelfStatement
      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }
    if (arraySize > 0)
      for (i <- 0 until arraySize)
        addStatements(s"[$i]")
    else
      addStatements("")

    if (hasChoice) {
      val initChoiceStatement = s"        break;"
      val parseStatement = s"        break;"
      val unparseStatement = s"        break;"

      structs.top.initChoiceStatements += initChoiceStatement
      structs.top.parserStatements += parseStatement
      structs.top.unparserStatements += unparseStatement
    }
  }

  def pushComplexElement(context: ElementBase): Unit = {
    val C = cStructName(context)
    structs.push(new ComplexCGState(C))
  }

  def popComplexElement(): Unit = {
    structs.pop()
  }

  // Gets length from explicit length declaration if any, otherwise from base type's implicit length
  private def getLengthInBits(e: ElementBase): Long = {
    // Skip HexBinary elements since some of them won't have a constant length
    if (e.optPrimType.get == PrimType.HexBinary)
      0
    else {
      e.schemaDefinitionUnless(e.elementLengthInBitsEv.isConstant, "Runtime dfdl:length expressions are not supported.")
      e.elementLengthInBitsEv.constValue.get
    }
  }

  // Gets element's primitive type while overriding type if needed to hold element's length
  private def getPrimType(e: ElementBase): PrimType = {
    val lengthInBits = getLengthInBits(e)
    val primType = e.optPrimType.get match {
      case PrimType.Boolean =>
        lengthInBits match {
          case n if n <= 32 => PrimType.Boolean
          case _ => e.SDE("Boolean lengths longer than 32 bits are not supported.")
        }
      case PrimType.Double
           | PrimType.Float =>
        lengthInBits match {
          case 32 => PrimType.Float
          case 64 => PrimType.Double
          case _ =>  e.SDE("Floating point lengths other than 32 or 64 bits are not supported.")
        }
      case PrimType.HexBinary => PrimType.HexBinary
      case PrimType.Byte
         | PrimType.Short
         | PrimType.Int
         | PrimType.Long
         | PrimType.Integer =>
        lengthInBits match {
          case n if n <= 8 =>  PrimType.Byte
          case n if n <= 16 => PrimType.Short
          case n if n <= 32 => PrimType.Int
          case n if n <= 64 => PrimType.Long
          case _ =>  e.SDE("Integer lengths longer than 64 bits are not supported.")
        }
      case PrimType.UnsignedByte
         | PrimType.UnsignedShort
         | PrimType.UnsignedInt
         | PrimType.UnsignedLong
         | PrimType.NonNegativeInteger =>
        lengthInBits match {
          case n if n <= 8 =>  PrimType.UnsignedByte
          case n if n <= 16 => PrimType.UnsignedShort
          case n if n <= 32 => PrimType.UnsignedInt
          case n if n <= 64 => PrimType.UnsignedLong
          case _ =>  e.SDE("Unsigned integer lengths longer than 64 bits are not supported.")
        }
      case p => e.SDE("PrimType %s is not supported in C code generator.", p.toString)
    }
    if (primType != e.optPrimType.get
      && e.optPrimType.get != PrimType.Integer
      && e.optPrimType.get != PrimType.NonNegativeInteger) {
      e.SDW(WarnID.IgnoreDFDLProperty, "Ignoring PrimType %s, using %s", e.optPrimType.get.toString, primType.toString)
    }
    primType
  }

  def addSimpleTypeERD(context: ElementBase): Unit = {
    val erd = erdName(context)
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
    val erdDef =
      s"""static const ERD $erd = {
         |$qNameInit
         |    $typeCode, // typeCode
         |    0, NULL, NULL, NULL, NULL, NULL
         |};
         |""".stripMargin
    erds += erdDef
  }

  def addComputations(child: ElementBase): Unit = {
    val C = structs.top.C
    val e = child.name
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
    val e = child.name
    val arrayDef = if (child.occursCountKind == OccursCountKind.Fixed) s"[${child.maxOccurs}]" else ""
    val indent = if (structs.top.initChoiceStatements.nonEmpty) "    " else ""
    val declaration = s"$indent    $definition $e$arrayDef;"

    structs.top.declarations += declaration

    // Add an array member to store a fixed length hexBinary element if needed
    if (child.isSimpleType && child.isFixedLength && child.optPrimType.get == PrimType.HexBinary
        && child.maybeFixedLengthInBits.get > 0) {
      val fixedLength = child.maybeFixedLengthInBits.get / 8
      val declaration2 = s"$indent    uint8_t     _a_$e$arrayDef[$fixedLength];"
      structs.top.declarations += declaration2
    }
  }

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

  def generateCodeHeader: String = {
    val structs = finalStructs.mkString("\n")
    val header =
      s"""#ifndef GENERATED_CODE_H
         |#define GENERATED_CODE_H
         |
         |// clang-format off
         |#include <stdbool.h>  // for bool
         |#include <stddef.h>   // for size_t
         |#include <stdint.h>   // for uint8_t, int16_t, int32_t, int64_t, uint32_t, int8_t, uint16_t, uint64_t
         |#include "infoset.h"  // for InfosetBase, HexBinary
         |// clang-format on
         |
         |// Define infoset structures
         |
         |$structs
         |#endif // GENERATED_CODE_H
         |""".stripMargin
    header.replace("\r\n", "\n").replace("\n", System.lineSeparator)
  }

  def generateCodeFile: String = {
    val rootName = cStructName(root)
    val prototypes = this.prototypes.mkString("\n")
    val erds = this.erds.mkString("\n")
    val finalImplementation = this.finalImplementation.mkString("\n")
    val code =
      s"""// clang-format off
         |#include "generated_code.h"
         |#include <math.h>       // for NAN
         |#include <stdbool.h>    // for false, bool, true
         |#include <stddef.h>     // for NULL, size_t
         |#include <string.h>     // for memset, memcmp
         |#include "errors.h"     // for Error, PState, UState, ERR_CHOICE_KEY, Error::(anonymous), UNUSED
         |#include "parsers.h"    // for alloc_hexBinary, parse_hexBinary, parse_be_float, parse_be_int16, parse_validate_fixed, parse_be_bool32, parse_be_bool16, parse_be_int32, parse_be_uint16, parse_be_uint32, parse_le_bool32, parse_le_int64, parse_le_uint16, parse_le_uint8, parse_be_bool8, parse_be_double, parse_be_int64, parse_be_int8, parse_be_uint64, parse_be_uint8, parse_le_bool16, parse_le_bool8, parse_le_double, parse_le_float, parse_le_int16, parse_le_int32, parse_le_int8, parse_le_uint32, parse_le_uint64
         |#include "unparsers.h"  // for unparse_hexBinary, unparse_be_float, unparse_be_int16, unparse_validate_fixed, unparse_be_bool32, unparse_be_bool16, unparse_be_int32, unparse_be_uint16, unparse_be_uint32, unparse_le_bool32, unparse_le_int64, unparse_le_uint16, unparse_le_uint8, unparse_be_bool8, unparse_be_double, unparse_be_int64, unparse_be_int8, unparse_be_uint64, unparse_be_uint8, unparse_le_bool16, unparse_le_bool8, unparse_le_double, unparse_le_float, unparse_le_int16, unparse_le_int32, unparse_le_int8, unparse_le_uint32, unparse_le_uint64
         |// clang-format on
         |
         |// Declare prototypes for easier compilation
         |
         |$prototypes
         |
         |// Define metadata for the infoset
         |
         |$erds
         |// Initialize, parse, and unparse nodes of the infoset
         |
         |$finalImplementation
         |// Return a root element for parsing or unparsing the infoset
         |
         |InfosetBase *
         |rootElement(void)
         |{
         |    static bool initialized;
         |    static $rootName root;
         |    if (!initialized)
         |    {
         |        ${rootName}_initERD(&root);
         |        ${rootName}_initSelf(&root);
         |        initialized = true;
         |    }
         |    return &root._base;
         |}
         |""".stripMargin
    code.replace("\r\n", "\n").replace("\n", System.lineSeparator)
  }
}

/**
 * Accumulates strings of generated C code for nested elements inside
 * complex elements.
 */
class ComplexCGState(val C: String) {
  val declarations: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val offsetComputations: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val erdComputations: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val initERDStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val initSelfStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val initChoiceStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val parserStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val unparserStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
}
