#ifndef ARGP_CODE_H
#define ARGP_CODE_H

#include <argp.h>

// Get our "daffodil parse" CLI options

struct daffodil_parse_cli
{
    const char *infoset_type;
    const char *infile;
    const char *outfile;
};
extern struct daffodil_parse_cli daffodil_parse;

// Get our "daffodil unparse" CLI options

struct daffodil_unparse_cli
{
    const char *infoset_type;
    const char *infile;
    const char *outfile;
};
extern struct daffodil_unparse_cli daffodil_unparse;

// Get our "daffodil" CLI options

struct daffodil_cli
{
    enum subcommand
    {
        NONE,
        PARSE,
        UNPARSE
    } subcommand;
    int verbosity;
};
extern struct daffodil_cli daffodil_cli;

// Parse our "daffodil" command line interface

extern error_t parse_daffodil_cli(int argc, char **argv);

#endif // ARGP_CODE_H
