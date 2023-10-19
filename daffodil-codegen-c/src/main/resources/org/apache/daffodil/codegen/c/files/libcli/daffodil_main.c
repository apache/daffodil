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

// auto-maintained by iwyu
// clang-format off
#include <stdbool.h>          // for bool, true
#include <stdio.h>            // for NULL, FILE, perror, fclose, fopen, stdin, stdout
#include <string.h>           // for strcmp
#include "cli_errors.h"       // for CLI_DIAGNOSTICS, CLI_FILE_CLOSE, CLI_FILE_OPEN
#include "daffodil_getopt.h"  // for daffodil_cli, daffodil_pu, daffodil_pu_cli, DAFFODIL_PARSE, DAFFODIL_UNPARSE
#include "errors.h"           // for continue_or_exit, Diagnostics, print_diagnostics, Error
#include "infoset.h"          // for ParserOrUnparserState, PState, UState, get_infoset, walk_infoset, parse_data, unparse_infoset, InfosetBase, VisitEventHandler
#include "xml_reader.h"       // for xmlReaderMethods, XMLReader
#include "xml_writer.h"       // for xmlWriterMethods, XMLWriter
// clang-format on

// Open a file or exit if it can't be opened

static FILE *
fopen_or_exit(FILE *stream, const char *pathname, const char *mode)
{
    if (strcmp(pathname, "-") != 0)
    {
        stream = fopen(pathname, mode);
        if (!stream)
        {
            perror("fopen");
            const Error error = {CLI_FILE_OPEN, {.s = pathname}};
            continue_or_exit(&error);
        }
    }
    return stream;
}

// Close a file or exit if it can't be closed

static void
fclose_or_exit(FILE *stream, FILE *stdin_or_stdout)
{
    if (stream != stdin_or_stdout && fclose(stream) != 0)
    {
        perror("fclose");
        const Error error = {CLI_FILE_CLOSE, {0}};
        continue_or_exit(&error);
    }
}

// Define our main entry point

int
main(int argc, char *argv[])
{
    // Parse our command line options
    const Error *error = parse_daffodil_cli(argc, argv);
    continue_or_exit(error);

    // We will read from stdin and write to stdout unless overridden
    FILE *input = stdin;
    FILE *output = stdout;

    // Perform our command
    if (daffodil_cli.subcommand == DAFFODIL_PARSE)
    {
        // Open our input and output files if given as arguments
        input = fopen_or_exit(input, daffodil_pu.infile, "r");
        output = fopen_or_exit(output, daffodil_pu.outfile, "w");

        // Parse the input file into our infoset
        const bool CLEAR_INFOSET = true;
        InfosetBase *infoset = get_infoset(CLEAR_INFOSET);
        PState pstate = {{input, 0, NULL, NULL}, 0, 0};
        parse_data(infoset, &pstate);
        print_diagnostics(pstate.pu.diagnostics);
        continue_or_exit(pstate.pu.error);

        // Visit the infoset and print XML from it
        XMLWriter xmlWriter = {xmlWriterMethods, output, {NULL, NULL, 0}};
        error = walk_infoset((VisitEventHandler *)&xmlWriter, infoset);
        continue_or_exit(error);

        // Any diagnostics will fail the parse if validate mode is on
        if (daffodil_pu.validate && pstate.pu.diagnostics)
        {
            const Error error = {CLI_DIAGNOSTICS, {.d64 = pstate.pu.diagnostics->length}};
            continue_or_exit(&error);
        }
    }
    else if (daffodil_cli.subcommand == DAFFODIL_UNPARSE)
    {
        // Open our input and output files if given as arguments
        input = fopen_or_exit(input, daffodil_pu.infile, "r");
        output = fopen_or_exit(output, daffodil_pu.outfile, "w");

        // Initialize our infoset's values from the XML data
        const bool CLEAR_INFOSET = true;
        InfosetBase *infoset = get_infoset(CLEAR_INFOSET);
        XMLReader xmlReader = {xmlReaderMethods, input, NULL, NULL};
        error = walk_infoset((VisitEventHandler *)&xmlReader, infoset);
        continue_or_exit(error);

        // Unparse our infoset to the output file
        UState ustate = {{output, 0, NULL, NULL}, 0, 0};
        unparse_infoset(infoset, &ustate);
        print_diagnostics(ustate.pu.diagnostics);
        continue_or_exit(ustate.pu.error);

        // Any diagnostics will fail the unparse if validate mode is on
        if (daffodil_pu.validate && ustate.pu.diagnostics)
        {
            const Error error = {CLI_DIAGNOSTICS, {.d64 = ustate.pu.diagnostics->length}};
            continue_or_exit(&error);
        }
    }

    // Close our input and out files if we opened them
    fclose_or_exit(input, stdin);
    fclose_or_exit(output, stdout);

    return 0;
}
