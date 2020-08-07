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
    val offsetComputations = structs.top.offsetComputations.mkString(",\n")
    val erdComputations = structs.top.erdComputations.mkString(",\n")
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
    val C = context.namedQName.local
    structs.push(new ComplexCGState(C))
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
    val C = structs.top.C
    val offsetComputation = s"	(void*)&${C}_compute_ERD_offsets.$e - (void*)&${C}_compute_ERD_offsets"
    val erdComputation = s"	&${e}ERD"
    structs.top.offsetComputations += offsetComputation
    structs.top.erdComputations += erdComputation
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
         |  // Parse command line options
         |  error_t status = global_cmd(argc, argv);
         |  if (status == 0) {
         |    FILE *input = stdin;
         |    FILE *output = stdout;
         |
         |    // Read our input from stdin or a filename argument.
         |    if (strcmp(parse.infile, "-")) {
         |      input = fopen(parse.infile, "r");
         |      if (input == NULL)
         |        {
         |          perror("Error opening input file: ");
         |          return EXIT_FAILURE;
         |        }
         |    }
         |
         |    // Write our output to stdout or a filename argument.
         |    if (strcmp(parse.outfile, "-")) {
         |      output = fopen(parse.outfile, "w");
         |      if (output == NULL)
         |        {
         |          perror("Error opening output file: ");
         |          return EXIT_FAILURE;
         |        }
         |    }
         |
         |    // Parse the input into our infoset.
         |    PState pstate = { input };
         |    XMLWriter xmlWriter = { xmlWriterMethods, output };
         |    $C instance = { { &${C}ERD }, 0 };
         |    ${C}ERD.parseSelf((InfosetBase*)&instance, &pstate);
         |
         |    // Visit the infoset and print XML from it.
         |    InfosetBase *infoNode = &instance._base;
         |    xml_init(&xmlWriter);
         |    visit_node_self((VisitEventHandler *)&xmlWriter, infoNode);
         |
         |    // Close the input if we opened it from a filename argument.
         |    if (input != stdin && fclose(input) != 0)
         |      {
         |        perror("Error closing input file: ");
         |        return EXIT_FAILURE;
         |      }
         |
         |    // Close the output if we opened it from a filename argument.
         |    if (output != stdout && fclose(output) != 0)
         |      {
         |        perror("Error closing output file: ");
         |        return EXIT_FAILURE;
         |      }
         |
         |    // Return success if we got this far.
         |    return EXIT_SUCCESS;
         |  }
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
         |#include <string.h>
         |#include <unistd.h>
         |#include "argp_code.h"
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

  def viewArgpHeader: String = {
    """#ifndef ARGP_CODE_H
       |#define ARGP_CODE_H
       |
       |#include <argp.h>
       |
       |// Externally callable prototype
       |
       |extern error_t global_cmd(int argc, char **argv);
       |
       |// Global options
       |
       |struct global_config {
       |  int verbosity;
       |};
       |extern struct global_config global;
       |
       |// Parse options
       |
       |struct parse_config {
       |  char *infoset_type;
       |  char *infile;
       |  char *outfile;
       |};
       |extern struct parse_config parse;
       |
       |#endif // ARGP_CODE_H
       |""".stripMargin
  }

  def viewArgpFile: String = {
    """#include <assert.h>
       |#include <stdio.h>
       |#include <stdlib.h>
       |#include <string.h>
       |#include "argp_code.h"
       |
       |// Name of our program
       |
       |const char *argp_program_version = "exe 0.0";
       |
       |// Locally callable prototype
       |
       |static error_t parse_cmd(struct argp_state *state);
       |
       |// Global options
       |
       |static struct argp_option global_options[] = {
       |    {"verbose", 'v', 0, 0, "Increment verbosity level, one level for each -v",
       |     -1},
       |
       |    {0}};
       |
       |static char global_args_doc[] = "<subcommand> [SUBCOMMAND_OPTION...]";
       |
       |static char global_doc[] =
       |    "\n"
       |    "Global Options:"
       |    "\v"
       |    "Subcommands:\n"
       |    "  parse         Parse data to a DFDL infoset\n"
       |    "\n"
       |    "Run 'argp-parse <subcommand> --help' for subcommand specific options";
       |
       |static error_t global_parser(int key, char *arg, struct argp_state *state) {
       |  struct global_config *global = state->input;
       |  error_t status = 0;
       |
       |  switch (key) {
       |  case 'v':
       |    global->verbosity++;
       |    break;
       |
       |  case ARGP_KEY_ARG:
       |    assert(arg);
       |    if (strcmp(arg, "parse") == 0) {
       |      status = parse_cmd(state);
       |    } else {
       |      argp_error(state, "%s is not a valid command", arg);
       |    }
       |    break;
       |
       |  default:
       |    return ARGP_ERR_UNKNOWN;
       |  }
       |
       |  return status;
       |}
       |
       |static struct argp global_argp = {
       |    global_options,
       |    global_parser,
       |    global_args_doc,
       |    global_doc,
       |};
       |
       |struct global_config global = {
       |    0, /* default verbosity */
       |};
       |
       |error_t global_cmd(int argc, char **argv) {
       |  return argp_parse(&global_argp, argc, argv, ARGP_IN_ORDER, NULL, &global);
       |}
       |
       |// Parse options
       |
       |static struct argp_option parse_options[] = {
       |    {"infoset-type", 'I', "<infoset_type>", 0,
       |     "Infoset type to output, must be one of 'xml'"},
       |
       |    {"output", 'o', "<file>", 0, "Write output to a given file"},
       |
       |    {0}};
       |
       |static char parse_args_doc[] = "[infile]";
       |
       |static char parse_doc[] = "\n"
       |                          "Parse a file\n"
       |                          "\n"
       |                          "Parse Options:"
       |                          "\v"
       |                          "Trailing arguments:\n"
       |                          " infile (not required)   input file to parse. If "
       |                          "not specified, or a value\n"
       |                          "                         of -, reads from stdin";
       |
       |static error_t parse_parser(int key, char *arg, struct argp_state *state) {
       |  struct parse_config *parse = state->input;
       |  assert(parse);
       |
       |  switch (key) {
       |  case 'I':
       |    parse->infoset_type = arg;
       |    break;
       |
       |  case 'o':
       |    parse->outfile = arg;
       |    break;
       |
       |  case ARGP_KEY_ARG:
       |    assert(arg);
       |    parse->infile = arg;
       |    break;
       |
       |  default:
       |    return ARGP_ERR_UNKNOWN;
       |  }
       |
       |  return 0;
       |}
       |
       |static struct argp parse_argp = {parse_options, parse_parser, parse_args_doc,
       |                                 parse_doc};
       |
       |struct parse_config parse = {"xml", "-", "-"};
       |
       |static error_t parse_cmd(struct argp_state *state) {
       |  int argc = state->argc - state->next + 1;
       |  char **argv = &state->argv[state->next - 1];
       |  char *argv0 = argv[0];
       |
       |  argv[0] = malloc(strlen(state->name) + strlen(" parse") + 1);
       |  if (!argv[0])
       |    argp_failure(state, 1, ENOMEM, 0);
       |  sprintf(argv[0], "%s parse", state->name);
       |
       |  error_t status =
       |      argp_parse(&parse_argp, argc, argv, ARGP_IN_ORDER, &argc, &parse);
       |
       |  free(argv[0]);
       |  argv[0] = argv0;
       |  state->next += argc - 1;
       |
       |  return status;
       |}
       |""".stripMargin
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

class ComplexCGState(val C: String) {
  val declarations: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val parserStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val unparserStatements: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val offsetComputations: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
  val erdComputations: mutable.ArrayBuffer[String] = mutable.ArrayBuffer[String]()
}
