#ifndef ARGP_CODE_H
#define ARGP_CODE_H

#include <argp.h>

// Externally callable prototype

extern error_t global_cmd(int argc, char **argv);

// Global options

struct global_config {
  int verbosity;
};
extern struct global_config global;

// Parse options

struct parse_config {
  char *infoset_type;
  char *infile;
  char *outfile;
};
extern struct parse_config parse;

#endif // ARGP_CODE_H
