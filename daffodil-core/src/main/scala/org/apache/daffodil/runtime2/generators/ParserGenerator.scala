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

import org.apache.daffodil.api.DFDL
import org.apache.daffodil.dpath.NodeInfo
import org.apache.daffodil.dpath.NodeInfo.PrimType
import org.apache.daffodil.dsom.ElementBase
import org.apache.daffodil.exceptions.ThrowsSDE

import scala.collection.mutable

trait ParserGenerator {
  // TBD: if the code-generator state builds up content by side-effect, there's no
  // reason to be returning it. It should return Unit.
  def generateCode(state: CodeGeneratorState): Unit
}

/**
 * Builds up the state of generated code.
 */
class CodeGeneratorState(private val code: String) extends DFDL.CodeGeneratorState {
  private val structs = mutable.Stack[ComplexCGState]()
  private val prototypes = mutable.ArrayBuffer[String]()
  private val erds = mutable.ArrayBuffer[String]()
  private val finalStructs = mutable.ArrayBuffer[String]()
  private val finalImplementation = mutable.ArrayBuffer[String]()

  def this() = this(null)

  def addParser(context: ElementBase): Unit = {
    val C = context.namedQName.local
    val parserStatements = structs.top.parserStatements.mkString("\n")
    val unparserStatements = structs.top.unparserStatements.mkString("\n")
    val prototypeFunctions =
      s"""void ${C}_parse_self($C *instance, PState *pstate);
         |void ${C}_unparse_self($C *instance, UState *ustate);
         |$C *${C}_new_instance();""".stripMargin
    prototypes += prototypeFunctions
    val functions =
      s"""void ${C}_parse_self($C *instance, PState *pstate)
         |{
         |$parserStatements
         |}
         |
         |void ${C}_unparse_self($C *instance, UState *ustate)
         |{
         |$unparserStatements
         |}
         |
         |$C *${C}_new_instance()
         |{
         |	$C *instance = calloc(sizeof($C), 1);
         |	// If InfosetBase adds more members, we need to set them too
         |	instance->_base.erd = &${C}ERD;
         |	return instance;
         |}""".stripMargin
    finalImplementation += functions
  }

  def addComplexTypeERD(context: ElementBase): Unit = {
    val C = context.namedQName.local
    val count = structs.top.declarations.length
    val offsetComputations = s"	(void*)&${C}_compute_ERD_offsets.e1 - (void*)&${C}_compute_ERD_offsets"
    val erdComputations = "	&e1ERD"
    val complexERD =
      s"""$C ${C}_compute_ERD_offsets;
         |
         |size_t ${C}_offsets[$count] = {
         |$offsetComputations
         |};
         |
         |ERD* ${C}_childrenERDs[$count] = {
         |$erdComputations
         |};
         |
         |ERD ${C}ERD =
         |{
         |	{ "$C" },			// namedQName
         |	COMPLEX,			// typeCode
         |	$count,					// count_children
         |	${C}_offsets,			// offsets
         |	${C}_childrenERDs,		// childrenERDs
         |	(Parse_Self)&${C}_parse_self,		// parseSelf
         |	(Unparse_Self)&${C}_unparse_self,	// unparseSelf
         |	(New_Instance)&${C}_new_instance	// newInstance
         |};
         |""".stripMargin
    erds += complexERD
  }

  def addStruct(context: ElementBase): Unit = {
    val C = context.namedQName.local
    val declarations = structs.top.declarations.mkString("\n")
    val struct =
      s"""typedef struct $C
         |{
         |	InfosetBase _base;
         |$declarations
         |} $C;
         |""".stripMargin
    finalStructs += struct
  }

  def addParseStatement(parseStatement: String): Unit = {
    structs.top.parserStatements += parseStatement
  }

  def addUnparseStatement(unparseStatement: String): Unit = {
    structs.top.unparserStatements += unparseStatement
  }

  def pushComplexElement(context: ElementBase): Unit = {
    structs.push(new ComplexCGState())
  }

  def popComplexElement(context: ElementBase): Unit = {
    structs.pop()
  }

  def addSimpleTypeERD(context: ElementBase): Unit = {
    val e = context.namedQName.local
    val typeCode = context.optPrimType.get match {
      case PrimType.Int => "PRIMITIVE_INT"
      case PrimType.String => "PRIMITIVE_STRING"
      case p: PrimType => context.SDE("PrimType %s not supported yet.", p.toString)
    }
    val erd =
      s"""ERD ${e}ERD =
         |{
         |	{ "$e" }, 			// namedQName
         |	$typeCode,		// typeCode
         |	0,					// count_children
         |	NULL,				// offsets
         |	NULL,				// childrenERDs
         |	NULL,				// parseSelf
         |	NULL,				// unparseSelf
         |	NULL				// newInstance
         |};
         |""".stripMargin
    erds += erd
  }

  def toPrimitive(primType: NodeInfo.PrimType, context: ThrowsSDE): String = {
    import NodeInfo.PrimType
    primType match {
      case PrimType.Long => "long"
      case PrimType.Int => "int"
      case _ => context.SDE("Unsupported primitive type: " + primType)
    }
  }

  def addFieldDeclaration(definition: String, name: String): Unit = {
    structs.top.declarations += s"	$definition $name;"
  }

  def viewCode: String = code

  def viewCodeHeader: String = {
    val structs = finalStructs.mkString("\n")
    val header =
      s"""#ifndef GENERATED_DATA_H
         |#define GENERATED_DATA_H
         |
         |#include "common_runtime.h"
         |
         |$structs
         |#endif // GENERATED_DATA_H
         |""".stripMargin
    header
  }

  def generateMain(rootElementName: String): String = {
    val C = rootElementName
    val main =
      s"""int main(int argc, char *argv[])
         |{
         |	// Read our input from stdin or a filename argument.
         |	FILE *stream = stdin;
         |	if (argc == 2)
         |	{
         |		stream = fopen(argv[1], "r");
         |		if (stream == NULL)
         |		{
         |			perror("Error opening file: ");
         |			return EXIT_FAILURE;
         |		}
         |	}
         |
         |	// Parse the input stream into our infoset.
         |	PState pstate = { stream };
         |	XMLWriter xmlWriter = { xmlWriterMethods, stdout };
         |	$C instance = { { &${C}ERD }, 0 };
         |	${C}ERD.parseSelf((InfosetBase*)&instance, &pstate);
         |
         |	// Close the input stream if we opened it from a filename argument.
         |	if (stream != stdin && fclose(stream) != 0)
         |	{
         |		perror("Error closing file: ");
         |		return EXIT_FAILURE;
         |	}
         |
         |	// Visit the infoset and print XML from it.
         |	InfosetBase *infoNode = &instance._base;
         |	xml_init(&xmlWriter);
         |	visit_node_self((VisitEventHandler *)&xmlWriter, infoNode);
         |
         |	// Return success if we got this far.
         |	return EXIT_SUCCESS;
         |}""".stripMargin
    main
  }

  def viewCodeFile(rootElementName: String): String = {
    val prototypes = this.prototypes.mkString("\n")
    val erds = this.erds.mkString("\n")
    val finalImplementation = this.finalImplementation.mkString("\n")
    val main = generateMain(rootElementName)
    val code =
      s"""#include <endian.h>
         |#include <stdio.h>
         |#include <stdlib.h>
         |#include <unistd.h>
         |#include "generated_data.h"
         |#include "xml_writer.h"
         |
         |// Function prototypes to allow compilation
         |
         |$prototypes
         |
         |// Metadata singletons
         |
         |$erds
         |
         |// Methods to parse, unparse, and create objects
         |
         |$finalImplementation
         |
         |// Main entry point
         |
         |$main
         |""".stripMargin
    code
  }

  def viewRuntimeHeader: String = {
    s"""#ifndef COMMON_RUNTIME_H
       |#define COMMON_RUNTIME_H
       |
       |#include <stdbool.h>
       |#include <stdint.h>
       |#include <stdio.h>
       |
       |// PState - parser state while parsing input
       |
       |typedef struct PState
       |{
       |	FILE *stream; // input to read from
       |} PState;
       |
       |// UState - unparser state while unparsing infoset
       |
       |typedef struct UState
       |{
       |	FILE *stream; // output to write to
       |} UState;
       |
       |// GlobalQName - name of an infoset element
       |
       |typedef struct GlobalQName
       |{
       |	char *name;
       |} GlobalQName;
       |
       |// TypeCode - type of an infoset element
       |
       |enum TypeCode
       |{
       |	COMPLEX,
       |	PRIMITIVE_INT
       |};
       |
       |// ERD - element runtime data needed to parse/unparse objects
       |
       |typedef struct ElementRuntimeData ERD;
       |typedef struct InfosetBase InfosetBase;
       |typedef void (*Parse_Self)(InfosetBase *infoNode, PState *pstate);
       |typedef void (*Unparse_Self)(InfosetBase *infoNode, UState *ustate);
       |typedef InfosetBase *(*New_Instance)();
       |
       |typedef struct ElementRuntimeData
       |{
       |	GlobalQName namedQName;
       |	enum TypeCode typeCode;
       |	uint32_t count_children;
       |	size_t *offsets;
       |	ERD **childrenERDs;
       |
       |	Parse_Self parseSelf;
       |	Unparse_Self unparseSelf;
       |	New_Instance newInstance;
       |} ERD;
       |
       |// VisitEventHandler - infoset visitor methods (generic)
       |
       |typedef struct VisitEventHandler VisitEventHandler;
       |typedef void (*VisitStart)(VisitEventHandler *handler, InfosetBase *base);
       |typedef void (*VisitEnd)(VisitEventHandler *handler, InfosetBase *base);
       |typedef void (*VisitInt)(VisitEventHandler *handler, ERD *erd, int *location);
       |
       |typedef struct VisitEventHandler
       |{
       |	VisitStart visitStart;
       |	VisitEnd visitEnd;
       |	VisitInt visitInt;
       |} VisitEventHandler;
       |
       |// InfosetBase - representation of an infoset element
       |
       |typedef struct InfosetBase
       |{
       |	ERD *erd;
       |} InfosetBase;
       |
       |// Generic visit method
       |
       |void visit_node_self(VisitEventHandler *handler, InfosetBase *infoNode);
       |
       |#endif // COMMON_RUNTIME_H
       |""".stripMargin
  }

  def viewRuntimeFile: String = {
    s"""#include <stdlib.h>
       |#include "common_runtime.h"
       |
       |// Generic method to visit infoset objects with a visit handler
       |
       |void visit_node_self(VisitEventHandler *handler, InfosetBase *infoNode)
       |{
       |	handler->visitStart(handler, infoNode);
       |	// Iterate through children...
       |	int count = infoNode->erd->count_children;
       |	ERD **childrenERDs = infoNode->erd->childrenERDs;
       |	size_t *offsets = infoNode->erd->offsets;
       |	for (int i = 0; i < count; i++)
       |	{
       |		size_t offset = offsets[i];
       |		ERD* childERD = childrenERDs[i];
       |		// NOTE: This can't be right - both childNode and intLocation get the same value
       |		InfosetBase *childNode = (InfosetBase *)((char *)infoNode + offset);
       |		int *intLocation = (int *)((char *)infoNode + offset);
       |
       |		// Need to handle more element types...
       |		enum TypeCode typeCode = childERD->typeCode;
       |		switch (typeCode)
       |		{
       |		case COMPLEX:
       |			visit_node_self(handler, childNode);
       |			break;
       |		case PRIMITIVE_INT:
       |			handler->visitInt(handler, childERD, intLocation);
       |			break;
       |		}
       |	}
       |	handler->visitEnd(handler, infoNode);
       |}
       |""".stripMargin
  }

  def viewWriterHeader: String = {
    s"""#ifndef XML_WRITER_H
       |#define XML_WRITER_H
       |
       |#include "common_runtime.h"
       |
       |// XMLWriter - infoset visitor with methods to output XML
       |
       |typedef struct XMLWriter
       |{
       |	VisitEventHandler handler;
       |	FILE *stream;
       |} XMLWriter;
       |
       |extern VisitEventHandler xmlWriterMethods;
       |
       |void xml_init(XMLWriter *writer);
       |
       |#endif // XML_WRITER_H
       |""".stripMargin
  }

  def viewWriterFile: String = {
    s"""#include <stdio.h>
       |#include "xml_writer.h"
       |
       |void xml_init(XMLWriter *writer)
       |{
       |	fprintf(writer->stream, u8"<?xml version=\\"1.0\\" encoding=\\"UTF-8\\"?>\\n");
       |}
       |
       |void xml_start(XMLWriter *writer, InfosetBase *base)
       |{
       |	char* name = base->erd->namedQName.name;
       |	fprintf(writer->stream, u8"<%s>\\n", name);
       |}
       |
       |void xml_end(XMLWriter *writer, InfosetBase *base)
       |{
       |	char* name = base->erd->namedQName.name;
       |	fprintf(writer->stream, u8"</%s>\\n", name);
       |}
       |
       |void xml_int(XMLWriter *writer, ERD *erd, int *location)
       |{
       |	char* name = erd->namedQName.name;
       |	int value = *location;
       |	fprintf(writer->stream, u8"  <%s>%i</%s>\\n", name, value, name);
       |}
       |
       |VisitEventHandler xmlWriterMethods = {
       |	(VisitStart)&xml_start,
       |	(VisitEnd)&xml_end,
       |	(VisitInt)&xml_int};
       |""".stripMargin
  }
}

class ComplexCGState() {
  val declarations: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val parserStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val unparserStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
}
