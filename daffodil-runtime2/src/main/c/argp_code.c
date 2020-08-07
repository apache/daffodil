#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "argp_code.h"

// Name of our program

const char *argp_program_version = "exe 0.0";

// Locally callable prototype

static error_t parse_cmd(struct argp_state *state);

// Global options

static struct argp_option global_options[] = {
    {"verbose", 'v', 0, 0, "Increment verbosity level, one level for each -v",
     -1},

    {0}};

static char global_args_doc[] = "<subcommand> [SUBCOMMAND_OPTION...]";

static char global_doc[] =
    "\n"
    "Global Options:"
    "\v"
    "Subcommands:\n"
    "  parse         Parse data to a DFDL infoset\n"
    "\n"
    "Run 'argp-parse <subcommand> --help' for subcommand specific options";

static error_t global_parser(int key, char *arg, struct argp_state *state) {
  struct global_config *global = state->input;
  error_t status = 0;

  switch (key) {
  case 'v':
    global->verbosity++;
    break;

  case ARGP_KEY_ARG:
    assert(arg);
    if (strcmp(arg, "parse") == 0) {
      status = parse_cmd(state);
    } else {
      argp_error(state, "%s is not a valid command", arg);
    }
    break;

  default:
    return ARGP_ERR_UNKNOWN;
  }

  return status;
}

static struct argp global_argp = {
    global_options,
    global_parser,
    global_args_doc,
    global_doc,
};

struct global_config global = {
    0, /* default verbosity */
};

error_t global_cmd(int argc, char **argv) {
  return argp_parse(&global_argp, argc, argv, ARGP_IN_ORDER, NULL, &global);
}

// Parse options

static struct argp_option parse_options[] = {
    {"infoset-type", 'I', "<infoset_type>", 0,
     "Infoset type to output, must be one of 'xml'"},

    {"output", 'o', "<file>", 0, "Write output to a given file"},

    {0}};

static char parse_args_doc[] = "[infile]";

static char parse_doc[] = "\n"
                          "Parse a file\n"
                          "\n"
                          "Parse Options:"
                          "\v"
                          "Trailing arguments:\n"
                          " infile (not required)   input file to parse. If "
                          "not specified, or a value\n"
                          "                         of -, reads from stdin";

static error_t parse_parser(int key, char *arg, struct argp_state *state) {
  struct parse_config *parse = state->input;
  assert(parse);

  switch (key) {
  case 'I':
    parse->infoset_type = arg;
    break;

  case 'o':
    parse->outfile = arg;
    break;

  case ARGP_KEY_ARG:
    assert(arg);
    parse->infile = arg;
    break;

  default:
    return ARGP_ERR_UNKNOWN;
  }

  return 0;
}

static struct argp parse_argp = {parse_options, parse_parser, parse_args_doc,
                                 parse_doc};

struct parse_config parse = {"xml", "-", "-"};

static error_t parse_cmd(struct argp_state *state) {
  int argc = state->argc - state->next + 1;
  char **argv = &state->argv[state->next - 1];
  char *argv0 = argv[0];

  argv[0] = malloc(strlen(state->name) + strlen(" parse") + 1);
  if (!argv[0])
    argp_failure(state, 1, ENOMEM, 0);
  sprintf(argv[0], "%s parse", state->name);

  error_t status =
      argp_parse(&parse_argp, argc, argv, ARGP_IN_ORDER, &argc, &parse);

  free(argv[0]);
  argv[0] = argv0;
  state->next += argc - 1;

  return status;
}
