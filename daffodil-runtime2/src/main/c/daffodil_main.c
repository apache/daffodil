#include "common_runtime.h" // for walkInfoset, InfosetBase, ERD, ...
#include "daffodil_argp.h"  // for daffodil_cli, parse_daffodil_cli, ...
#include "generated_code.h" // for rootInfoset
#include "xml_reader.h"     // for xmlReaderMethods, XMLReader
#include "xml_writer.h"     // for xmlWriterMethods, XMLWriter
#include <error.h>          // for error
#include <stdio.h>          // for FILE, perror, fclose, fopen, NULL, stdin
#include <stdlib.h>         // for exit, EXIT_FAILURE
#include <string.h>         // for strcmp

// Open a file or exit if it can't be opened

static FILE *
fopen_or_exit(FILE *stream, const char *pathname, const char *mode)
{
    if (strcmp(pathname, "-") != 0)
    {
        stream = fopen(pathname, mode);
        if (stream == NULL)
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
    if (error_msg != NULL)
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
        InfosetBase *root = rootInfoset();

        if (daffodil_cli.subcommand == DAFFODIL_PARSE)
        {
            // Open our input and output files if given as arguments.
            input = fopen_or_exit(input, daffodil_parse.infile, "r");
            output = fopen_or_exit(output, daffodil_parse.outfile, "w");

            // Parse the input file into our infoset.
            PState      pstate = {input};
            const char *error_msg = root->erd->parseSelf(root, &pstate);
            continue_or_exit(error_msg);

            // Visit the infoset and print XML from it.
            XMLWriter xmlWriter = {xmlWriterMethods, output};
            error_msg = walkInfoset((VisitEventHandler *)&xmlWriter, root);
            continue_or_exit(error_msg);
        }
        else if (daffodil_cli.subcommand == DAFFODIL_UNPARSE)
        {
            // Open our input and output files if given as arguments.
            input = fopen_or_exit(input, daffodil_unparse.infile, "r");
            output = fopen_or_exit(output, daffodil_unparse.outfile, "w");

            // Initialize our infoset's values from the XML data.
            XMLReader   xmlReader = {xmlReaderMethods, input, root};
            const char *error_msg =
                walkInfoset((VisitEventHandler *)&xmlReader, root);
            continue_or_exit(error_msg);

            // Unparse our infoset to the output file.
            UState ustate = {output};
            error_msg = root->erd->unparseSelf(root, &ustate);
            continue_or_exit(error_msg);
        }

        // Close our input and out files if we opened them.
        fclose_or_exit(input, stdin);
        fclose_or_exit(output, stdout);
    }

    return status;
}
