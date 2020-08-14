#include "argp_code.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Initialize our program name and version

const char *argp_program_version = "argp_code 0.1";

// Initialize our "parse" CLI options

struct parse_config parse = {
    "xml", // default infoset type
    "-",   // default infile
    "-",   // default outfile
};

static struct argp_option parse_options[] = {
    {"infoset-type", 'I', "<infoset_type>", 0,
     "Infoset type to output. Must be one of 'xml' or 'null'"},

    {"output", 'o', "<file>", 0,
     "Write output to a given file. If not given or is -, output is written to "
     "stdout"},

    {0}};

static error_t parse_parser(int key, char *arg, struct argp_state *state);

static char parse_args_doc[] = "[infile]";

static char parse_doc[] = "\n"
                          "Parse a file using a DFDL schema\n"
                          "\n"
                          "Parse Options:"
                          "\v"
                          " Trailing arguments:\n"
                          "  infile (not required)      input file to parse. "
                          "If not specified, or a value of -, reads from stdin";

static struct argp parse_argp = {
    parse_options,  // array of CLI options
    parse_parser,   // function to parse these CLI options
    parse_args_doc, // short usage documentation
    parse_doc,      // long help documentation
};

// Parse our "parse" CLI options

static error_t parse_parser(int key, char *arg, struct argp_state *state) {
  struct parse_config *parse = state->input;

  switch (key) {
  case 'I':
    parse->infoset_type = arg;
    break;

  case 'o':
    parse->outfile = arg;
    break;

  case ARGP_KEY_ARG:
    if (state->arg_num) {
      argp_error(state, "too many arguments: %s", arg);
    }
    parse->infile = arg;
    break;

  default:
    return ARGP_ERR_UNKNOWN;
  }

  return 0;
}

// Parse our "parse" command line interface

static error_t parse_subcli_parse(struct argp_state *state) {
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

// Initialize our "unparse" CLI options

struct unparse_config unparse = {
    "xml", // default infoset type
    "-",   // default infile
    "-",   // default outfile
};

static struct argp_option unparse_options[] = {
    {"infoset-type", 'I', "<infoset_type>", 0,
     "Infoset type to unparse. Must be 'xml'"},

    {"output", 'o', "<file>", 0,
     "Write output to file. If not given or is -, output is written to "
     "standard output"},

    {0}};

static error_t unparse_parser(int key, char *arg, struct argp_state *state);

static char unparse_args_doc[] = "[infile]";

static char unparse_doc[] =
    "\n"
    "Unparse an infoset file using a DFDL schema\n"
    "\n"
    "Unparse Options:"
    "\v"
    " Trailing arguments:\n"
    "  infile (not required)      input file to unparse. If not specified, or "
    "a value of -, reads from stdin";

static struct argp unparse_argp = {
    unparse_options,  // array of CLI options
    unparse_parser,   // function to parse these CLI options
    unparse_args_doc, // short usage documentation
    unparse_doc,      // long help documentation
};

// Parse our "unparse" CLI options

static error_t unparse_parser(int key, char *arg, struct argp_state *state) {
  struct unparse_config *unparse = state->input;

  switch (key) {
  case 'I':
    unparse->infoset_type = arg;
    break;

  case 'o':
    unparse->outfile = arg;
    break;

  case ARGP_KEY_ARG:
    if (state->arg_num) {
      argp_error(state, "too many arguments: %s", arg);
    }
    unparse->infile = arg;
    break;

  default:
    return ARGP_ERR_UNKNOWN;
  }

  return 0;
}

// Parse our "unparse" command line interface

static error_t parse_subcli_unparse(struct argp_state *state) {
  int argc = state->argc - state->next + 1;
  char **argv = &state->argv[state->next - 1];
  char *argv0 = argv[0];

  argv[0] = malloc(strlen(state->name) + strlen(" unparse") + 1);
  if (!argv[0])
    argp_failure(state, 1, ENOMEM, 0);
  sprintf(argv[0], "%s unparse", state->name);

  error_t status =
      argp_parse(&unparse_argp, argc, argv, ARGP_IN_ORDER, &argc, &unparse);

  free(argv[0]);
  argv[0] = argv0;
  state->next += argc - 1;

  return status;
}

// Initialize our "global" CLI options

struct global_config global = {
    NONE, // default subcommand
    0,    // default verbosity
};

static struct argp_option global_options[] = {
    {"verbose", 'v', 0, 0, "Increment verbosity level, one level for each -v",
     -1},

    {0}};

static error_t global_parser(int key, char *arg, struct argp_state *state);

static char global_args_doc[] = "<subcommand> [SUBCOMMAND_OPTION...]";

static char global_doc[] =
    "\n"
    "Global Options:"
    "\v"
    "Subcommands:\n"
    "  parse         Parse data to a DFDL infoset\n"
    "  unparse       Unparse a DFDL infoset\n"
    "\n"
    "Run 'argp_code <subcommand> --help' for subcommand specific options";

static struct argp global_argp = {
    global_options,  // array of CLI options
    global_parser,   // function to parse these CLI options
    global_args_doc, // short usage documentation
    global_doc,      // long help documentation
};

// Parse our "global" CLI options

static error_t global_parser(int key, char *arg, struct argp_state *state) {
  struct global_config *global = state->input;
  error_t status = 0;

  switch (key) {
  case 'v':
    global->verbosity++;
    break;

  case ARGP_KEY_ARG:
    if (strcmp(arg, "parse") == 0) {
      global->subcommand = PARSE;
      status = parse_subcli_parse(state);
    } else if (strcmp(arg, "unparse") == 0) {
      global->subcommand = UNPARSE;
      status = parse_subcli_unparse(state);
    } else {
      argp_error(state, "%s is not a valid subcommand", arg);
    }
    break;

  case ARGP_KEY_END:
    if (global->subcommand == NONE) {
      argp_error(state, "missing subcommand");
    }
    break;

  default:
    return ARGP_ERR_UNKNOWN;
  }

  return status;
}

// Parse our command line interface

static char *disable_note = "ARGP_HELP_FMT=no-dup-args-note";

error_t parse_cli(int argc, char **argv) {
  putenv(disable_note); // Do not pass an automatic variable to putenv
  return argp_parse(&global_argp, argc, argv, ARGP_IN_ORDER, NULL, &global);
}
