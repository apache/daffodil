#ifndef ARGP_CODE_H
#define ARGP_CODE_H

#include <argp.h>

// Get our "parse" CLI options

struct parse_config {
  char *infoset_type;
  char *infile;
  char *outfile;
};
extern struct parse_config parse;

// Get our "unparse" CLI options

struct unparse_config {
  char *infoset_type;
  char *infile;
  char *outfile;
};
extern struct unparse_config unparse;

// Get our "global" CLI options

struct global_config {
  enum subcommand { NONE, PARSE, UNPARSE } subcommand;
  int verbosity;
};
extern struct global_config global;

// Parse our command line interface

extern error_t parse_cli(int argc, char **argv);

#endif // ARGP_CODE_H
