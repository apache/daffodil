#ifndef DAFFODIL_ARGP_H
#define DAFFODIL_ARGP_H

#include <argp.h>

// Parse our "daffodil" command line interface

extern error_t parse_daffodil_cli(int argc, char **argv);

// Get our "daffodil" CLI options

extern struct daffodil_cli
{
    enum daffodil_subcommand
    {
        DAFFODIL_NONE,
        DAFFODIL_PARSE,
        DAFFODIL_UNPARSE
    } subcommand;
    int verbosity;
} daffodil_cli;

// Get our "daffodil parse" CLI options

extern struct daffodil_parse_cli
{
    const char *infoset_type;
    const char *infile;
    const char *outfile;
} daffodil_parse;

// Get our "daffodil unparse" CLI options

extern struct daffodil_unparse_cli
{
    const char *infoset_type;
    const char *infile;
    const char *outfile;
} daffodil_unparse;

#endif // DAFFODIL_ARGP_H
