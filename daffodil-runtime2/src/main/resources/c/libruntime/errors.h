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

#include <stdio.h>    // for FILE, size_t
#include <stdint.h>   // for int64_t

// ErrorCode - types of errors which could occur

enum ErrorCode
{
    ERR_CHOICE_KEY,
    ERR_FILE_CLOSE,
    ERR_FILE_FLUSH,
    ERR_FILE_OPEN,
    ERR_FIXED_VALUE,
    ERR_INFOSET_READ,
    ERR_INFOSET_WRITE,
    ERR_PARSE_BOOL,
    ERR_STACK_EMPTY,
    ERR_STACK_OVERFLOW,
    ERR_STACK_UNDERFLOW,
    ERR_STREAM_EOF,
    ERR_STREAM_ERROR,
    ERR_STRTOBOOL,
    ERR_STRTOD_ERRNO,
    ERR_STRTOI_ERRNO,
    ERR_STRTONUM_EMPTY,
    ERR_STRTONUM_NOT,
    ERR_STRTONUM_RANGE,
    ERR_XML_DECL,
    ERR_XML_ELEMENT,
    ERR_XML_ERD,
    ERR_XML_GONE,
    ERR_XML_INPUT,
    ERR_XML_LEFT,
    ERR_XML_MISMATCH,
    ERR_XML_WRITE
};

// Error - specific error occuring now

typedef struct Error
{
    enum ErrorCode code;
    union
    {
        const char *s;   // for %s
        int64_t     d64; // for %d64
    };
} Error;

// Diagnostics - array of validation errors

typedef struct Diagnostics
{
    Error  array[100];
    size_t length;
} Diagnostics;

// PState - mutable state while parsing data

typedef struct PState
{
    FILE *       stream;   // input to read data from
    size_t       position; // 0-based position in stream
    Diagnostics *validati; // any validation diagnostics
    const Error *error;    // any error which stops program
} PState;

// UState - mutable state while unparsing infoset

typedef struct UState
{
    FILE *       stream;   // output to write data to
    size_t       position; // 0-based position in stream
    Diagnostics *validati; // any validation diagnostics
    const Error *error;    // any error which stops program
} UState;

// need_diagnostics - return pointer to validation diagnostics

extern Diagnostics *need_diagnostics(void);

// print_diagnostics - print any validation diagnostics

extern void print_diagnostics(const Diagnostics *validati);

// continue_or_exit - print and exit if an error occurred or continue otherwise

extern void continue_or_exit(const Error *error);

// eof_or_error - return an error if a stream has its eof or error indicator set

extern const Error *eof_or_error(FILE *stream);

// UNUSED - suppress compiler warning about unused variable

#define UNUSED(x) (void)(x)

#endif // ERRORS_H
