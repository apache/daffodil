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

#ifndef ERRORS_H
#define ERRORS_H

// auto-maintained by iwyu
// clang-format off
#include <stdbool.h>  // for bool
#include <stddef.h>   // for size_t
#include <stdint.h>   // for uint8_t, int64_t
#include <stdio.h>    // for FILE
// clang-format on

// ErrorCode - identifiers of libruntime errors

enum ErrorCode
{
    ERR_ARRAY_BOUNDS,
    ERR_CHOICE_KEY,
    ERR_HEXBINARY_ALLOC,
    ERR_LEFTOVER_DATA,
    ERR_PARSE_BOOL,
    ERR_RESTR_ENUM,
    ERR_RESTR_FIXED,
    ERR_RESTR_RANGE,
    ERR_STREAM_EOF,
    ERR_STREAM_ERROR,
    ERR__NUM_CODES,
};

// ErrorField - identifiers of Error fields

enum ErrorField
{
    FIELD_C,
    FIELD_D64,
    FIELD_S,
    FIELD_S_ON_STDOUT,
    FIELD__NO_ARGS,
};

// ErrorLookup - structure of an error lookup table row

typedef struct ErrorLookup
{
    uint8_t code;
    const char *message;
    enum ErrorField field;
} ErrorLookup;

// Error - specific error occuring now

typedef struct Error
{
    uint8_t code;
    union
    {
        int c;         // for %c
        int64_t d64;   // for %d64
        const char *s; // for %s
    } arg;
} Error;

// Limits - limits on how many elements static arrays can hold

enum Limits
{
    LIMIT_DIAGNOSTICS = 100,  // limits how many diagnostics can accumulate
    LIMIT_NAME_LENGTH = 9999, // limits how long infoset names can become
};

// Diagnostics - array of validation errors

typedef struct Diagnostics
{
    Error array[LIMIT_DIAGNOSTICS];
    size_t length;
} Diagnostics;

// eof_or_error - get pointer to error if stream has eof or error indicator set

extern const Error *eof_or_error(FILE *stream);

// get_diagnostics - get pointer to validation diagnostics

extern Diagnostics *get_diagnostics(void);

// add_diagnostic - add a new error to validation diagnostics

extern bool add_diagnostic(Diagnostics *diagnostics, const Error *error);

// print_diagnostics - print any validation diagnostics

extern void print_diagnostics(const Diagnostics *diagnostics);

// continue_or_exit - print and exit if any error or continue otherwise

extern void continue_or_exit(const Error *error);

// cli_error_lookup - declare our pluggable error lookup mechanism

typedef const ErrorLookup *cli_error_lookup_t(uint8_t code);
extern cli_error_lookup_t *cli_error_lookup;

#endif // ERRORS_H
