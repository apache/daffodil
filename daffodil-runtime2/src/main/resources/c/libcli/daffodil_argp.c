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

#include "daffodil_argp.h"
#include <argp.h>    // for argp_state, argp_error, error_t, argp_parse, ARGP_ERR_UNKNOWN, ARGP_IN_ORDER, ARGP_KEY_ARG, argp, argp_option, ARGP_KEY_END
#include <stdio.h>   // for sprintf, NULL
#include <stdlib.h>  // for putenv
#include <string.h>  // for strlen, strcmp

// Initialize our "daffodil" name and version

const char *argp_program_version = "Apache Daffodil (runtime2) 0.1";

// Initialize our "daffodil parse" CLI options

struct daffodil_parse_cli daffodil_parse = {
    "xml", // default infoset type
    "-",   // default infile
    "-",   // default outfile
};

static const struct argp_option parse_options[] = {
    {"infoset-type", 'I', "<infoset_type>", 0,
     "Infoset type to output. Must be one of 'xml' or 'null'", 0},

    {"output", 'o', "<file>", 0,
     "Write output to a given file. If not given or is -, output is written to "
     "stdout",
     0},

    {0}};

static error_t parse_handler(int key, char *arg, struct argp_state *state);

static const char parse_args_doc[] = "[infile]";

static const char parse_doc[] =
    "\n"
    "Parse a file using a DFDL schema\n"
    "\n"
    "Parse Options:"
    "\v"
    " Trailing arguments:\n"
    "  infile (not required)      input file to parse. "
    "If not specified, or a value of -, reads from stdin";

static const struct argp parse_argp = {
    parse_options,  // array of CLI options
    parse_handler,  // function to get these CLI options
    parse_args_doc, // short usage documentation
    parse_doc,      // long help documentation
    0,              // child argps parsed after this argp
    0,              // function to replace help messages
    0               // domain name for translation lookup
};

// Handle callbacks to get our "daffodil parse" CLI options

static error_t
parse_handler(int key, char *arg, struct argp_state *state)
{
    struct daffodil_parse_cli *parse = state->input;

    switch (key)
    {
    case 'I':
        parse->infoset_converter = arg;
        break;

    case 'o':
        parse->outfile = arg;
        break;

    case ARGP_KEY_ARG:
        if (state->arg_num)
        {
            argp_error(state, "too many arguments: %s", arg);
        }
        parse->infile = arg;
        break;

    default:
        return ARGP_ERR_UNKNOWN;
    }

    return 0;
}

// Parse our "daffodil parse" command line interface

static error_t
parse_daffodil_parse_cli(struct argp_state *state)
{
    int    argc = state->argc - state->next + 1;
    char **argv = &state->argv[state->next - 1];
    char * old_cmd = argv[0];
    char   new_cmd[strlen(state->name) + strlen(" parse") + 1];

    sprintf(new_cmd, "%s parse", state->name);
    argv[0] = new_cmd;

    error_t status = argp_parse(&parse_argp, argc, argv, ARGP_IN_ORDER, &argc,
                                &daffodil_parse);

    argv[0] = old_cmd;
    state->next += argc - 1;

    return status;
}

// Initialize our "daffodil unparse" CLI options

struct daffodil_unparse_cli daffodil_unparse = {
    "xml", // default infoset type
    "-",   // default infile
    "-",   // default outfile
};

static const struct argp_option unparse_options[] = {
    {"infoset-type", 'I', "<infoset_type>", 0,
     "Infoset type to unparse. Must be 'xml'", 0},

    {"output", 'o', "<file>", 0,
     "Write output to file. If not given or is -, output is written to "
     "standard output",
     0},

    {0}};

static error_t unparse_handler(int key, char *arg, struct argp_state *state);

static const char unparse_args_doc[] = "[infile]";

static const char unparse_doc[] =
    "\n"
    "Unparse an infoset file using a DFDL schema\n"
    "\n"
    "Unparse Options:"
    "\v"
    " Trailing arguments:\n"
    "  infile (not required)      input file to unparse. If not specified, or "
    "a value of -, reads from stdin";

static const struct argp unparse_argp = {
    unparse_options,  // array of CLI options
    unparse_handler,  // function to get these CLI options
    unparse_args_doc, // short usage documentation
    unparse_doc,      // long help documentation
    0,                // child argps parsed after this argp
    0,                // function to replace help messages
    0                 // domain name for translation lookup
};

// Handle callbacks to get our "daffodil unparse" CLI options

static error_t
unparse_handler(int key, char *arg, struct argp_state *state)
{
    struct daffodil_unparse_cli *unparse = state->input;

    switch (key)
    {
    case 'I':
        unparse->infoset_converter = arg;
        break;

    case 'o':
        unparse->outfile = arg;
        break;

    case ARGP_KEY_ARG:
        if (state->arg_num)
        {
            argp_error(state, "too many arguments: %s", arg);
        }
        unparse->infile = arg;
        break;

    default:
        return ARGP_ERR_UNKNOWN;
    }

    return 0;
}

// Parse our "daffodil unparse" command line interface

static error_t
parse_daffodil_unparse_cli(struct argp_state *state)
{
    int    argc = state->argc - state->next + 1;
    char **argv = &state->argv[state->next - 1];
    char * old_cmd = argv[0];
    char   new_cmd[strlen(state->name) + strlen(" unparse") + 1];

    sprintf(new_cmd, "%s unparse", state->name);
    argv[0] = new_cmd;

    error_t status = argp_parse(&unparse_argp, argc, argv, ARGP_IN_ORDER, &argc,
                                &daffodil_unparse);

    argv[0] = old_cmd;
    state->next += argc - 1;

    return status;
}

// Initialize our "daffodil" CLI options

struct daffodil_cli daffodil_cli = {
    DAFFODIL_NONE, // default subcommand
    0,             // default verbosity
};

static const struct argp_option daffodil_options[] = {
    {"verbose", 'v', 0, 0, "Increment verbosity level, one level for each -v",
     -1},

    {0}};

static error_t daffodil_handler(int key, char *arg, struct argp_state *state);

static const char daffodil_args_doc[] = "<subcommand> [SUBCOMMAND_OPTION...]";

static const char daffodil_doc[] =
    "\n"
    "Global Options:"
    "\v"
    "Subcommands:\n"
    "  parse         Parse data to a DFDL infoset\n"
    "  unparse       Unparse a DFDL infoset\n"
    "\n"
    "Run 'daffodil <subcommand> --help' for subcommand specific options";

static const struct argp daffodil_argp = {
    daffodil_options,  // array of CLI options
    daffodil_handler,  // function to get these CLI options
    daffodil_args_doc, // short usage documentation
    daffodil_doc,      // long help documentation
    0,                 // child argps parsed after this argp
    0,                 // function to replace help messages
    0                  // domain name for translation lookup
};

// Handle callbacks to get our "daffodil" CLI options

static error_t
daffodil_handler(int key, char *arg, struct argp_state *state)
{
    struct daffodil_cli *cli = state->input;
    error_t              status = 0;

    switch (key)
    {
    case 'v':
        cli->verbosity++;
        break;

    case ARGP_KEY_ARG:
        if (strcmp(arg, "parse") == 0)
        {
            cli->subcommand = DAFFODIL_PARSE;
            status = parse_daffodil_parse_cli(state);
        }
        else if (strcmp(arg, "unparse") == 0)
        {
            cli->subcommand = DAFFODIL_UNPARSE;
            status = parse_daffodil_unparse_cli(state);
        }
        else
        {
            argp_error(state, "%s is not a valid subcommand", arg);
        }
        break;

    case ARGP_KEY_END:
        if (cli->subcommand == DAFFODIL_NONE)
        {
            argp_error(state, "missing subcommand");
        }
        break;

    default:
        return ARGP_ERR_UNKNOWN;
    }

    return status;
}

// Parse our "daffodil" command line interface

error_t
parse_daffodil_cli(int argc, char **argv)
{
    static char *argp_help_fmt = "ARGP_HELP_FMT=no-dup-args-note";
    putenv(argp_help_fmt); // Do not pass an automatic variable to putenv
    return argp_parse(&daffodil_argp, argc, argv, ARGP_IN_ORDER, NULL,
                      &daffodil_cli);
}
