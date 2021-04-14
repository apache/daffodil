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

#include <stdio.h>          // for NULL, perror, FILE, fclose, fflush, fopen, stdin, stdout
#include <string.h>         // for strcmp
#include "daffodil_argp.h"  // for daffodil_parse, daffodil_parse_cli, daffodil_unparse, daffodil_unparse_cli, daffodil_cli, parse_daffodil_cli, DAFFODIL_PARSE, DAFFODIL_UNPARSE
#include "errors.h"         // for continue_or_exit, Error, print_diagnostics, PState, UState, ERR_FILE_CLOSE, ERR_FILE_FLUSH, ERR_FILE_OPEN, ERR_INFOSET_READ, ERR_INFOSET_WRITE
#include "infoset.h"        // for walkInfoset, InfosetBase, rootElement, ERD, VisitEventHandler
#include "xml_reader.h"     // for xmlReaderMethods, XMLReader
#include "xml_writer.h"     // for xmlWriterMethods, XMLWriter

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
            const Error error = {ERR_FILE_OPEN, {pathname}};
            continue_or_exit(&error);
        }
    }
    return stream;
}

// Flush all output to a file or exit if it can't be written, also
// print and exit if any previous error occurred or continue otherwise

static void
fflush_continue_or_exit(FILE *output, const Error *error)
{
    if (fflush(output) != 0)
    {
        perror("fflush");
        if (!error)
        {
            const Error error = {ERR_FILE_FLUSH, {NULL}};
            continue_or_exit(&error);
        }
    }
    continue_or_exit(error);
}

// Close a file or exit if it can't be closed

static void
fclose_or_exit(FILE *stream, FILE *stdin_or_stdout)
{
    if (stream != stdin_or_stdout && fclose(stream) != 0)
    {
        perror("fclose");
        const Error error = {ERR_FILE_CLOSE, {NULL}};
        continue_or_exit(&error);
    }
}

// Define our daffodil program's main entry point

int
main(int argc, char *argv[])
{
    // Parse our "daffodil" command line options
    int status = parse_daffodil_cli(argc, argv);

    if (status == 0)
    {
        FILE *       input = stdin;
        FILE *       output = stdout;
        InfosetBase *root = rootElement();

        if (daffodil_cli.subcommand == DAFFODIL_PARSE)
        {
            // Open our input and output files if given as arguments.
            input = fopen_or_exit(input, daffodil_parse.infile, "r");
            output = fopen_or_exit(output, daffodil_parse.outfile, "w");

            // Parse the input file into our infoset.
            PState pstate = {input, 0, NULL, NULL};
            root->erd->parseSelf(root, &pstate);
            print_diagnostics(pstate.diagnostics);
            continue_or_exit(pstate.error);

            if (strcmp(daffodil_parse.infoset_converter, "xml") == 0)
            {
                // Visit the infoset and print XML from it.
                XMLWriter    xmlWriter = {xmlWriterMethods, output, {NULL, NULL, 0}};
                const Error *error = walkInfoset((VisitEventHandler *)&xmlWriter, root);
                fflush_continue_or_exit(output, error);
            }
            else
            {
                const Error error = {ERR_INFOSET_WRITE, {daffodil_parse.infoset_converter}};
                continue_or_exit(&error);
            }
        }
        else if (daffodil_cli.subcommand == DAFFODIL_UNPARSE)
        {
            // Open our input and output files if given as arguments.
            input = fopen_or_exit(input, daffodil_unparse.infile, "r");
            output = fopen_or_exit(output, daffodil_unparse.outfile, "w");

            if (strcmp(daffodil_unparse.infoset_converter, "xml") == 0)
            {
                // Initialize our infoset's values from the XML data.
                XMLReader    xmlReader = {xmlReaderMethods, input, root, NULL, NULL};
                const Error *error = walkInfoset((VisitEventHandler *)&xmlReader, root);
                continue_or_exit(error);
            }
            else
            {
                const Error error = {ERR_INFOSET_READ, {daffodil_unparse.infoset_converter}};
                continue_or_exit(&error);
            }

            // Unparse our infoset to the output file.
            UState ustate = {output, 0, NULL, NULL};
            root->erd->unparseSelf(root, &ustate);
            print_diagnostics(ustate.diagnostics);
            continue_or_exit(ustate.error);
        }

        // Close our input and out files if we opened them.
        fclose_or_exit(input, stdin);
        fclose_or_exit(output, stdout);
    }

    return status;
}
