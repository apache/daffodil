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

import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.dsom.GlobalElementDecl
import org.apache.daffodil.dsom.SchemaComponent
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.schema.annotation.props.gen.OccursCountKind

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

  // Builds a name for the given element that needs to be unique in C file scope
  private def qualifiedName(context: ElementBase): String = {
    def buildName(sc: SchemaComponent, sb: StringBuilder): StringBuilder = {
      sc match {
        case gd: GlobalElementDecl => sb ++= gd.namedQName.local += '_'
        case eb: ElementBase => sb ++= eb.namedQName.local += '_'
        case _ => // don't include other schema components in qualified name
      }
      sc.optLexicalParent.foreach {
        buildName(_, sb)
      }
      sb
    }
    val sb = buildName(context, new StringBuilder)
    sb.toString()
  }

  // Returns the given element's local name (doesn't have to be unique)
  private def localName(context: ElementBase): String = context.namedQName.local

  def addImplementation(context: ElementBase): Unit = {
    val C = localName(context)
    val initStatements = structs.top.initStatements.mkString("\n")
    val parserStatements = structs.top.parserStatements.mkString("\n")
    val unparserStatements = structs.top.unparserStatements.mkString("\n")
    val prototypeFunctions =
      s"""static void ${C}_initSelf($C *instance);
         |static void ${C}_parseSelf($C *instance, PState *pstate);
         |static void ${C}_unparseSelf(const $C *instance, UState *ustate);""".stripMargin
    prototypes += prototypeFunctions
    val functions =
      s"""static void
         |${C}_initSelf($C *instance)
         |{
         |$initStatements
         |}
         |
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

  def addComplexTypeERD(context: ElementBase): Unit = {
    val C = localName(context)
    val qn = qualifiedName(context)
    val count = structs.top.offsetComputations.length
    val offsetComputations = structs.top.offsetComputations.mkString(",\n")
    val erdComputations = structs.top.erdComputations.mkString(",\n")
    val qnameInit = defineQNameInit(context)
    val complexERD =
      s"""static const $C ${C}_compute_offsets;
         |
         |static const ptrdiff_t ${C}_offsets[$count] = {
         |$offsetComputations
         |};
         |
         |static const ERD *${C}_childrenERDs[$count] = {
         |$erdComputations
         |};
         |
         |static const ERD ${qn}_ERD = {
         |$qnameInit
         |    COMPLEX,                         // typeCode
         |    $count,                               // numChildren
         |    ${C}_offsets,                      // offsets
         |    ${C}_childrenERDs,                 // childrenERDs
         |    (ERDInitSelf)&${C}_initSelf,       // initSelf
         |    (ERDParseSelf)&${C}_parseSelf,     // parseSelf
         |    (ERDUnparseSelf)&${C}_unparseSelf, // unparseSelf
         |};
         |""".stripMargin
    erds += complexERD
  }

  def addStruct(context: ElementBase): Unit = {
    val C = localName(context)
    val qn = qualifiedName(context)
    val declarations = structs.top.declarations.mkString("\n")
    val struct =
      s"""typedef struct $C
         |{
         |    InfosetBase _base;
         |$declarations
         |} $C;
         |""".stripMargin
    finalStructs += struct
    val initStatement = s"    instance->_base.erd = &${qn}_ERD;"
    structs.top.initStatements += initStatement
  }

  def addSimpleTypeStatements(initStatement: String, parseStatement: String, unparseStatement: String): Unit = {
    structs.top.initStatements += initStatement
    structs.top.parserStatements += parseStatement
    structs.top.unparserStatements += unparseStatement
  }

  def addComplexTypeStatements(child: ElementBase): Unit = {
    val C = localName(child)
    val e = child.name
    val initStatement = s"    ${C}_initSelf(&instance->$e);"
    val parseStatement = s"    ${C}_parseSelf(&instance->$e, pstate);"
    val unparseStatement = s"    ${C}_unparseSelf(&instance->$e, ustate);"
    structs.top.initStatements += initStatement
    structs.top.parserStatements += parseStatement
    structs.top.unparserStatements += unparseStatement
  }

  def pushComplexElement(context: ElementBase): Unit = {
    val C = localName(context)
    structs.push(new ComplexCGState(C))
  }

  def popComplexElement(context: ElementBase): Unit = {
    structs.pop()
  }

  def addSimpleTypeERD(context: ElementBase): Unit = {
    val qn = qualifiedName(context)
    val qnameInit = defineQNameInit(context)
    val typeCode = context.optPrimType.get match {
      case PrimType.UnsignedLong => "PRIMITIVE_UINT64"
      case PrimType.UnsignedInt => "PRIMITIVE_UINT32"
      case PrimType.UnsignedShort => "PRIMITIVE_UINT16"
      case PrimType.UnsignedByte => "PRIMITIVE_UINT8"
      case PrimType.Long => "PRIMITIVE_INT64"
      case PrimType.Int => "PRIMITIVE_INT32"
      case PrimType.Short => "PRIMITIVE_INT16"
      case PrimType.Byte => "PRIMITIVE_INT8"
      case PrimType.Float => "PRIMITIVE_FLOAT"
      case PrimType.Double => "PRIMITIVE_DOUBLE"
      case p: PrimType => context.SDE("PrimType %s not supported yet.", p.toString)
    }
    val erd =
      s"""static const ERD ${qn}_ERD = {
         |$qnameInit
         |    $typeCode, // typeCode
         |    0,               // numChildren
         |    NULL,            // offsets
         |    NULL,            // childrenERDs
         |    NULL,            // initSelf
         |    NULL,            // parseSelf
         |    NULL,            // unparseSelf
         |};
         |""".stripMargin
    erds += erd
    addComputations(context)
  }

  def addComputations(child: ElementBase): Unit = {
    val C = structs.top.C
    val e = localName(child)
    val qn = qualifiedName(child)
    val arraySize = if (child.occursCountKind == OccursCountKind.Fixed) child.maxOccurs else 0
    def addComputation(deref: String): Unit = {
      val offsetComputation = s"    (const char *)&${C}_compute_offsets.$e$deref - (const char *)&${C}_compute_offsets"
      val erdComputation = s"    &${qn}_ERD"
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
      import NodeInfo.PrimType
      child.optPrimType.get match {
        case PrimType.UnsignedLong => "uint64_t   "
        case PrimType.UnsignedInt => "uint32_t   "
        case PrimType.UnsignedShort => "uint16_t   "
        case PrimType.UnsignedByte => "uint8_t    "
        case PrimType.Long => "int64_t    "
        case PrimType.Int => "int32_t    "
        case PrimType.Short => "int16_t    "
        case PrimType.Byte => "int8_t     "
        case PrimType.Float => "float      "
        case PrimType.Double => "double     "
        case x => context.SDE("Unsupported primitive type: " + x)
      }
    } else {
      localName(child)
    }
    val e = child.name
    val arrayDef = if (child.occursCountKind == OccursCountKind.Fixed) s"[${child.maxOccurs}]" else ""
    structs.top.declarations += s"    $definition $e$arrayDef;"
  }

  def generateCodeHeader: String = {
    val structs = finalStructs.mkString("\n")
    val header =
      s"""#ifndef GENERATED_CODE_H
         |#define GENERATED_CODE_H
         |
         |#include "infoset.h"  // for InfosetBase
         |#include <stdint.h>   // for int16_t, int32_t, int64_t, int8_t, uint16_t, uint32_t, uint64_t, uint8_t

         |// Define some infoset structures
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
         |#include <stddef.h>     // for NULL, ptrdiff_t
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
         |extern InfosetBase *
         |rootElement(void)
         |{
         |    static $rootElementName instance;
         |    InfosetBase *root = &instance._base;
         |    ${rootElementName}__ERD.initSelf(root);
         |    return root;
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
  val parserStatements = mutable.ArrayBuffer[String]()
  val unparserStatements = mutable.ArrayBuffer[String]()
}
