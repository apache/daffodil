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

#include "errors.h"
#include <error.h>     // for error
#include <inttypes.h>  // for PRId64
#include <stdio.h>     // for NULL, feof, ferror, FILE, size_t
#include <stdlib.h>    // for EXIT_FAILURE

// error_message - return an internationalized error message

static const char *
error_message(enum ErrorCode code)
{
    switch (code)
    {
    case ERR_CHOICE_KEY:
        return "no match between choice dispatch key %" PRId64
               " and any branch key";
    case ERR_FILE_CLOSE:
        return "error closing file";
    case ERR_FILE_FLUSH:
        return "error flushing stream to file";
    case ERR_FILE_OPEN:
        return "error opening file '%s'";
    case ERR_FIXED_VALUE:
        return "value of element '%s' does not match value of its "
               "'fixed' attribute";
    case ERR_INFOSET_READ:
        return "cannot read infoset type '%s'";
    case ERR_INFOSET_WRITE:
        return "cannot write infoset type '%s'";
    case ERR_PARSE_BOOL:
        return "error parsing binary value %" PRId64 " as either true or false";
    case ERR_STACK_EMPTY:
        return "stack empty, stopping program";
    case ERR_STACK_OVERFLOW:
        return "stack overflow, stopping program";
    case ERR_STACK_UNDERFLOW:
        return "stack underflow, stopping program";
    case ERR_STREAM_EOF:
        return "EOF in stream, stopping program";
    case ERR_STREAM_ERROR:
        return "error in stream, stopping program";
    case ERR_STRTOBOOL:
        return "error converting XML data '%s' to boolean";
    case ERR_STRTOD_ERRNO:
        return "error converting XML data '%s' to number";
    case ERR_STRTOI_ERRNO:
        return "error converting XML data '%s' to integer";
    case ERR_STRTONUM_EMPTY:
        return "found no number in XML data '%s'";
    case ERR_STRTONUM_NOT:
        return "found non-number characters in XML data '%s'";
    case ERR_STRTONUM_RANGE:
        return "number in XML data '%s' out of range";
    case ERR_XML_DECL:
        return "error making new XML declaration";
    case ERR_XML_ELEMENT:
        return "error making new XML element '%s'";
    case ERR_XML_ERD:
        return "unexpected ERD typeCode %" PRId64 " while reading XML data";
    case ERR_XML_GONE:
        return "ran out of XML data";
    case ERR_XML_INPUT:
        return "unable to read XML data from input file";
    case ERR_XML_LEFT:
        return "did not consume all of the XML data, '%s' left";
    case ERR_XML_MISMATCH:
        return "found mismatch between XML data and infoset '%s'";
    case ERR_XML_WRITE:
        return "error writing XML document";
    default:
        return "unrecognized error code, shouldn't happen";
    }
}

// print_maybe_stop - print a message and maybe stop the program

static void
print_maybe_stop(const Error *err, int status)
{
    const int   errnum = 0;
    const char *format = "%s";
    const char *msg = error_message(err->code);

    switch (err->code)
    {
    case ERR_FILE_OPEN:
    case ERR_FIXED_VALUE:
    case ERR_INFOSET_READ:
    case ERR_INFOSET_WRITE:
    case ERR_STRTOBOOL:
    case ERR_STRTOD_ERRNO:
    case ERR_STRTOI_ERRNO:
    case ERR_STRTONUM_EMPTY:
    case ERR_STRTONUM_NOT:
    case ERR_STRTONUM_RANGE:
    case ERR_XML_ELEMENT:
    case ERR_XML_LEFT:
    case ERR_XML_MISMATCH:
        error(status, errnum, msg, err->s);
        break;
    case ERR_CHOICE_KEY:
    case ERR_PARSE_BOOL:
    case ERR_XML_ERD:
        error(status, errnum, msg, err->d64);
        break;
    default:
        error(status, errnum, format, msg);
        break;
    }
}

// need_diagnostics - return pointer to validation diagnostics

Diagnostics *
need_diagnostics(void)
{
    static Diagnostics validati;
    return &validati;
}

// add_diagnostic - add a new error to validation diagnostics

void
add_diagnostic(Diagnostics *validati, const Error *error)
{
    if (validati && error)
    {
        if (validati->length <
            sizeof(validati->array) / sizeof(*validati->array))
        {
            Error *err = &validati->array[validati->length++];
            err->code = error->code;
            err->s = error->s;
        }
    }
}

// print_diagnostics - print any validation diagnostics

void
print_diagnostics(const Diagnostics *validati)
{
    if (validati)
    {
        for (size_t i = 0; i < validati->length; i++)
        {
            const Error *error = &validati->array[i];
            print_maybe_stop(error, 0);
        }
    }
}

// continue_or_exit - print and exit if an error occurred or continue otherwise

void
continue_or_exit(const Error *error)
{
    if (error)
    {
        print_maybe_stop(error, EXIT_FAILURE);
    }
}

// eof_or_error - return an error if a stream has its eof or error indicator set

const Error *
eof_or_error(FILE *stream)
{
    if (feof(stream))
    {
        static Error error = {ERR_STREAM_EOF, {NULL}};
        return &error;
    }
    else if (ferror(stream))
    {
        static Error error = {ERR_STREAM_ERROR, {NULL}};
        return &error;
    }
    else
    {
        return NULL;
    }
}
