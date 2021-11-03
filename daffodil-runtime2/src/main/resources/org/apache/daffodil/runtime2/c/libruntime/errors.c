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
#include "errors.h"
#include <assert.h>    // for assert
#include <inttypes.h>  // for PRId64
#include <stdbool.h>   // for bool, false, true
#include <stdio.h>     // for fprintf, stderr, feof, ferror, FILE, stdout
#include <stdlib.h>    // for exit, EXIT_FAILURE, EXIT_SUCCESS
// clang-format oon

// eof_or_error - get pointer to error if stream has eof or error indicator set

const Error *
eof_or_error(FILE *stream)
{
    if (feof(stream))
    {
        static Error error = {ERR_STREAM_EOF, {0}};
        return &error;
    }
    else if (ferror(stream))
    {
        static Error error = {ERR_STREAM_ERROR, {0}};
        return &error;
    }
    else
    {
        return NULL;
    }
}

// get_diagnostics - get pointer to validation diagnostics

Diagnostics *
get_diagnostics(void)
{
    static Diagnostics diagnostics;
    return &diagnostics;
}

// add_diagnostic - add a new error to validation diagnostics

bool
add_diagnostic(Diagnostics *diagnostics, const Error *error)
{
    if (diagnostics && error)
    {
        if (diagnostics->length < LIMIT_DIAGNOSTICS)
        {
            Error *err = &diagnostics->array[diagnostics->length++];
            err->code = error->code;
            err->arg.s = error->arg.s;
            return true;
        }
    }
    return false;
}

// error_lookup - look up an internationalized error message

static const ErrorLookup *
error_lookup(uint8_t code)
{
    static const ErrorLookup table[ERR_ZZZ] = {
        {ERR_CHOICE_KEY, "no match between choice dispatch key %" PRId64 " and any branch key\n", FIELD_D64},
        {ERR_FIXED_VALUE, "value of element '%s' does not match value of its 'fixed' attribute\n", FIELD_S},
        {ERR_HEXBINARY_ALLOC, "error allocating hexBinary memory -- %" PRId64 " bytes\n", FIELD_D64},
        {ERR_PARSE_BOOL, "error parsing binary value %" PRId64 " as either true or false\n", FIELD_D64},
        {ERR_STREAM_EOF, "EOF in stream, stopping program\n", FIELD_ZZZ},
        {ERR_STREAM_ERROR, "error in stream, stopping program\n", FIELD_ZZZ},
    };

    if (code < ERR_ZZZ)
    {
        const ErrorLookup *lookup = &table[code];

        // Double check that we looked up correct row
        assert(code == lookup->code);

        return lookup;
    }
    else
    {
        return NULL;
    }
}

// print_maybe_stop - print a message and maybe stop the program

static void
print_maybe_stop(const Error *error, int status)
{
    const ErrorLookup *lookup = error_lookup(error->code);
    if (!lookup && cli_error_lookup)
    {
        lookup = cli_error_lookup(error->code);
    }
    assert(lookup);

    switch (lookup->field)
    {
    case FIELD_C:
        fprintf(stderr, lookup->message, error->arg.c);
        break;
    case FIELD_D64:
        fprintf(stderr, lookup->message, error->arg.d64);
        break;
    case FIELD_S:
        fprintf(stderr, lookup->message, error->arg.s);
        break;
    case FIELD_S_ON_STDOUT:
        fprintf(stdout, lookup->message, error->arg.s);
        exit(EXIT_SUCCESS);
        break;
    case FIELD_ZZZ:
    default:
        fprintf(stderr, "%s", lookup->message);
        break;
    }

    // Maybe stop the program
    if (status)
    {
        exit(status);
    }
}

// print_diagnostics - print any validation diagnostics

void
print_diagnostics(const Diagnostics *diagnostics)
{
    if (diagnostics)
    {
        for (size_t i = 0; i < diagnostics->length; i++)
        {
            const Error *error = &diagnostics->array[i];
            print_maybe_stop(error, 0);
        }
    }
}

// continue_or_exit - print and exit if any error or continue otherwise

void
continue_or_exit(const Error *error)
{
    if (error)
    {
        print_maybe_stop(error, EXIT_FAILURE);
    }
}

// check_error_lookup - call from debugger to check error lookup tables

uint8_t
check_error_lookup(void)
{
    uint8_t code = 0;
    const ErrorLookup *lookup = 0;
    do
    {
        lookup = error_lookup(code);
        if (!lookup && cli_error_lookup)
        {
            lookup = cli_error_lookup(code);
        }
    } while (lookup && ++code);

    return code;
}
