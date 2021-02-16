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

#include "daffodil_argp.h"  // for daffodil_cli, daffodil_parse, daffodil_parse_cli, daffodil_unparse, daffodil_unparse_cli, parse_daffodil_cli, DAFFODIL_PARSE, DAFFODIL_UNPARSE
#include "infoset.h"        // for walkInfoset, InfosetBase, rootElement, ERD, PState, UState, VisitEventHandler
#include "xml_reader.h"     // for xmlReaderMethods, XMLReader
#include "xml_writer.h"     // for xmlWriterMethods, XMLWriter
#include <error.h>          // for error
#include <stdio.h>          // for FILE, perror, fclose, fopen, stdin, stdout
#include <stdlib.h>         // for exit, EXIT_FAILURE
#include <string.h>         // for strcmp

// Open a file or exit if it can't be opened

static FILE *
fopen_or_exit(FILE *stream, const char *pathname, const char *mode)
{
    if (strcmp(pathname, "-") != 0)
    {
        stream = fopen(pathname, mode);
        if (!stream)
        {
            perror("Error opening file: ");
            exit(EXIT_FAILURE);
        }
    }
    return stream;
}

// Close a stream or exit if it can't be closed

static void
fclose_or_exit(FILE *stream, FILE *stdin_stdout)
{
    if (stream != stdin_stdout && fclose(stream) != 0)
    {
        perror("Error closing file: ");
        exit(EXIT_FAILURE);
    }
}

// Print an error and exit if an error occurred or continue otherwise

static void
continue_or_exit(const char *error_msg)
{
    if (error_msg)
    {
        error(EXIT_FAILURE, 0, "%s", error_msg);
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
            PState pstate = {input, 0, NULL};
            root->erd->parseSelf(root, &pstate);
            continue_or_exit(pstate.error_msg);

            if (strcmp(daffodil_parse.infoset_converter, "xml") == 0)
            {
                // Visit the infoset and print XML from it.
                XMLWriter   xmlWriter = {
                    xmlWriterMethods, output, {NULL, NULL, 0}};
                const char *error_msg =
                    walkInfoset((VisitEventHandler *)&xmlWriter, root);
                continue_or_exit(error_msg);
            }
            else
            {
                error(EXIT_FAILURE, 0, "Cannot write infoset type '%s'",
                      daffodil_parse.infoset_converter);
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
                XMLReader   xmlReader = {
                    xmlReaderMethods, input, root, NULL, NULL};
                const char *error_msg =
                    walkInfoset((VisitEventHandler *)&xmlReader, root);
                continue_or_exit(error_msg);
            }
            else
            {
                error(EXIT_FAILURE, 0, "Cannot read infoset type '%s'",
                      daffodil_unparse.infoset_converter);
            }

            // Unparse our infoset to the output file.
            UState ustate = {output, 0, NULL};
            root->erd->unparseSelf(root, &ustate);
            continue_or_exit(ustate.error_msg);
        }

        // Close our input and out files if we opened them.
        fclose_or_exit(input, stdin);
        fclose_or_exit(output, stdout);
    }

    return status;
}
