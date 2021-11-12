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
#include "cli_errors.h"
#include <assert.h>    // for assert
#include <inttypes.h>  // for PRId64, uint8_t
#include <stddef.h>    // for NULL
// clang-format on

// USAGE - second line to append to CLI usage messages

#define USAGE "Try using `-h` for more information\n"

// error_lookup - look up an internationalized error message

static const ErrorLookup *
error_lookup(uint8_t code)
{
    static const ErrorLookup table[CLI_ZZZ - ERR_ZZZ] = {
        {CLI_FILE_CLOSE, "error closing file\n", FIELD_ZZZ},
        {CLI_FILE_OPEN, "error opening file '%s'\n", FIELD_S},
        {CLI_HELP_USAGE,
         "Usage: %s [OPTION...] <command> [infile]\n"
         "\n"
         "Options:\n"
         "  -h            Give this help list\n"
         "  -I            Infoset type to write or read. Must be 'xml'\n"
         "  -o            Write output to file. If not given or is -,\n"
         "                output is written to stdout\n"
         "  -V            Print program version\n"
         "\n"
         "Commands:\n"
         "  parse         Parse a data file to an infoset file\n"
         "  unparse       Unparse an infoset file to a data file\n"
         "\n"
         "Argument:\n"
         "  infile        Input file to parse or unparse. If not specified,\n"
         "                or a value of -, reads from stdin\n",
         FIELD_S},
        {CLI_HEXBINARY_LENGTH, "odd hexBinary length -- %" PRId64 " nibbles\n", FIELD_D64},
        {CLI_HEXBINARY_PARSE, "invalid character in hexBinary -- '%c'\n", FIELD_C},
        {CLI_HEXBINARY_SIZE, "hexBinary too long -- only %" PRId64 " bytes allowed\n", FIELD_D64},
        {CLI_INVALID_COMMAND, "invalid command -- '%s'\n" USAGE, FIELD_S},
        {CLI_INVALID_INFOSET, "invalid infoset type -- '%s'\n" USAGE, FIELD_S},
        {CLI_INVALID_OPTION, "invalid option -- '%c'\n" USAGE, FIELD_C},
        {CLI_MISSING_COMMAND, "missing command\n" USAGE, FIELD_ZZZ},
        {CLI_MISSING_VALUE, "option requires an argument -- '%c'\n" USAGE, FIELD_C},
        {CLI_PROGRAM_ERROR,
         "unexpected getopt code %" PRId64 "\n"
         "Check for program error\n",
         FIELD_D64},
        {CLI_PROGRAM_VERSION, "%s\n", FIELD_S_ON_STDOUT},
        {CLI_STACK_EMPTY, "stack empty, stopping program\n", FIELD_ZZZ},
        {CLI_STACK_OVERFLOW, "stack overflow, stopping program\n", FIELD_ZZZ},
        {CLI_STACK_UNDERFLOW, "stack underflow, stopping program\n", FIELD_ZZZ},
        {CLI_STRTOBOOL, "error converting XML data '%s' to boolean\n", FIELD_S},
        {CLI_STRTOD_ERRNO, "error converting XML data '%s' to number\n", FIELD_S},
        {CLI_STRTOI_ERRNO, "error converting XML data '%s' to integer\n", FIELD_S},
        {CLI_STRTONUM_EMPTY, "found no number in XML data '%s'\n", FIELD_S},
        {CLI_STRTONUM_NOT, "found non-number characters in XML data '%s'\n", FIELD_S},
        {CLI_STRTONUM_RANGE, "number in XML data '%s' out of range\n", FIELD_S},
        {CLI_UNEXPECTED_ARGUMENT, "unexpected extra argument -- '%s'\n" USAGE, FIELD_S},
        {CLI_XML_DECL, "error making new XML declaration\n", FIELD_ZZZ},
        {CLI_XML_ELEMENT, "error making new XML element '%s'\n", FIELD_S},
        {CLI_XML_ERD, "unexpected ERD typeCode %" PRId64 " while reading XML data\n", FIELD_D64},
        {CLI_XML_GONE, "ran out of XML data\n", FIELD_ZZZ},
        {CLI_XML_INPUT, "unable to read XML data from input file\n", FIELD_ZZZ},
        {CLI_XML_LEFT, "did not consume all of the XML data, '%s' left\n", FIELD_S},
        {CLI_XML_MISMATCH, "found mismatch between XML data and infoset '%s'\n", FIELD_S},
        {CLI_XML_WRITE, "error writing XML document\n", FIELD_ZZZ},
    };

    if (code >= ERR_ZZZ && code < CLI_ZZZ)
    {
        const ErrorLookup *lookup = &table[code - ERR_ZZZ];

        // Double check that we looked up correct row
        assert(code == lookup->code);

        return lookup;
    }
    else
    {
        return NULL;
    }
}

// Initialize pluggable error lookup mechanism to let libruntime print
// our CLI errors as well as its own errors

cli_error_lookup_t *cli_error_lookup = &error_lookup;
