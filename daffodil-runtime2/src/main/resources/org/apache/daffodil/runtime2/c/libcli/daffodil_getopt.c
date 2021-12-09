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
#include "daffodil_getopt.h"
#include <string.h>            // for strcmp, strrchr
#include <unistd.h>            // for optarg, getopt, optopt, optind
#include "cli_errors.h"        // for CLI_UNEXPECTED_ARGUMENT, CLI_HELP_USAGE, CLI_INVALID_COMMAND, CLI_INVALID_INFOSET, CLI_INVALID_OPTION, CLI_MISSING_COMMAND, CLI_MISSING_VALUE, CLI_PROGRAM_ERROR, CLI_PROGRAM_VERSION
#include "daffodil_version.h"  // for daffodil_version
// clang-format on

// Initialize our "daffodil" CLI options

struct daffodil_cli daffodil_cli = {
    DAFFODIL_MISSING_COMMAND, // default subcommand
};

// Initialize our "daffodil parse" CLI options

struct daffodil_parse_cli daffodil_parse = {
    "xml", // default infoset type
    "-",   // default infile
    "-",   // default outfile
};

// Initialize our "daffodil unparse" CLI options

struct daffodil_unparse_cli daffodil_unparse = {
    "xml", // default infoset type
    "-",   // default infile
    "-",   // default outfile
};

// Parse our command line interface.  Note there is NO portable way to
// parse "daffodil [options] command [more options] arguments" with
// getopt.  We will have to put all options before all arguments,
// e.g., "daffodil [options] command arguments".

const Error *
parse_daffodil_cli(int argc, char *argv[])
{
    // Fill in and return if any error happens
    static Error error;

    // Get our executable's basename
    const char *exe = strrchr(argv[0], '/');
    exe = exe ? exe + 1 : argv[0];

    // We expect callers to put all non-option arguments at the end
    int opt = 0;
    while ((opt = getopt(argc, argv, ":hI:o:V")) != -1)
    {
        switch (opt)
        {
        case 'h':
            error.code = CLI_HELP_USAGE;
            error.arg.s = exe;
            return &error;
        case 'I':
            if (strcmp("xml", optarg) != 0)
            {
                error.code = CLI_INVALID_INFOSET;
                error.arg.s = optarg;
                return &error;
            }
            daffodil_parse.infoset_converter = optarg;
            daffodil_unparse.infoset_converter = optarg;
            break;
        case 'o':
            daffodil_parse.outfile = optarg;
            daffodil_unparse.outfile = optarg;
            break;
        case 'V':
            error.code = CLI_PROGRAM_VERSION;
            error.arg.s = daffodil_version;
            return &error;
        case ':':
            error.code = CLI_MISSING_VALUE;
            error.arg.c = optopt;
            return &error;
        case '?':
            error.code = CLI_INVALID_OPTION;
            error.arg.c = optopt;
            return &error;
        default:
            // shouldn't happen unless programmer made error
            error.code = CLI_PROGRAM_ERROR;
            error.arg.d64 = opt;
            return &error;
        }
    }

    // Get the command and the infile arg
    for (int i = optind; i < argc; i++)
    {
        const char *arg = argv[i];

        if (DAFFODIL_PARSE == daffodil_cli.subcommand)
        {
            if (strcmp("-", daffodil_parse.infile) == 0)
            {
                daffodil_parse.infile = arg;
            }
            else
            {
                error.code = CLI_UNEXPECTED_ARGUMENT;
                error.arg.s = arg;
                return &error;
            }
        }
        else if (DAFFODIL_UNPARSE == daffodil_cli.subcommand)
        {
            if (strcmp("-", daffodil_unparse.infile) == 0)
            {
                daffodil_unparse.infile = arg;
            }
            else
            {
                error.code = CLI_UNEXPECTED_ARGUMENT;
                error.arg.s = arg;
                return &error;
            }
        }
        else if (strcmp("parse", arg) == 0)
        {
            daffodil_cli.subcommand = DAFFODIL_PARSE;
        }
        else if (strcmp("unparse", arg) == 0)
        {
            daffodil_cli.subcommand = DAFFODIL_UNPARSE;
        }
        else
        {
            error.code = CLI_INVALID_COMMAND;
            error.arg.s = arg;
            return &error;
        }
    }

    if (DAFFODIL_MISSING_COMMAND == daffodil_cli.subcommand)
    {
        error.code = CLI_MISSING_COMMAND;
        error.arg.c = 0;
        return &error;
    }

    return 0;
}
