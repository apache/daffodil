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

// clang-format off
#include <stdio.h>            // for NULL, FILE, perror, fclose, fopen, stdin, stdout
#include <string.h>           // for strcmp
#include "cli_errors.h"       // for CLI_FILE_CLOSE, CLI_FILE_OPEN
#include "daffodil_getopt.h"  // for daffodil_cli, parse_daffodil_cli, daffodil_parse, daffodil_parse_cli, daffodil_unparse, daffodil_unparse_cli, DAFFODIL_PARSE, DAFFODIL_UNPARSE
#include "errors.h"           // for continue_or_exit, print_diagnostics, Error
#include "infoset.h"          // for walkInfoset, InfosetBase, PState, UState, flushUState, rootElement, ERD, VisitEventHandler
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

    // Get our infoset ready
    FILE *       input = stdin;
    FILE *       output = stdout;
    InfosetBase *root = rootElement();

    // Perform our command
    if (daffodil_cli.subcommand == DAFFODIL_PARSE)
    {
        // Open our input and output files if given as arguments
        input = fopen_or_exit(input, daffodil_parse.infile, "r");
        output = fopen_or_exit(output, daffodil_parse.outfile, "w");

        // Parse the input file into our infoset
        PState pstate = {input, 0, NULL, NULL, 0, 0};
        root->erd->parseSelf(root, &pstate);
        print_diagnostics(pstate.diagnostics);
        continue_or_exit(pstate.error);

        // Visit the infoset and print XML from it
        XMLWriter xmlWriter = {xmlWriterMethods, output, {NULL, NULL, 0}};
        error = walkInfoset((VisitEventHandler *)&xmlWriter, root);
        continue_or_exit(error);
    }
    else if (daffodil_cli.subcommand == DAFFODIL_UNPARSE)
    {
        // Open our input and output files if given as arguments
        input = fopen_or_exit(input, daffodil_unparse.infile, "r");
        output = fopen_or_exit(output, daffodil_unparse.outfile, "w");

        // Initialize our infoset's values from the XML data
        XMLReader xmlReader = {xmlReaderMethods, input, root, NULL, NULL};
        error = walkInfoset((VisitEventHandler *)&xmlReader, root);
        continue_or_exit(error);

        // Unparse our infoset to the output file
        UState ustate = {output, 0, NULL, NULL, 0, 0};
        root->erd->unparseSelf(root, &ustate);
        flushUState(&ustate);
        print_diagnostics(ustate.diagnostics);
        continue_or_exit(ustate.error);
    }

    // Close our input and out files if we opened them
    fclose_or_exit(input, stdin);
    fclose_or_exit(output, stdout);

    return 0;
}
