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

#include "parsers.h"
#include <endian.h>   // for be32toh, be64toh, le32toh, le64toh, be16toh, le16toh
#include <stdbool.h>  // for bool
#include <stdio.h>    // for fread, size_t

// Macros that are not defined by <endian.h>

#define be8toh(var) var

#define le8toh(var) var

// Helper macro to reduce duplication of C code reading stream,
// updating position, and checking for errors

#define read_stream_update_position                                            \
    size_t count = fread(&buffer.c_val, 1, sizeof(buffer), pstate->stream);    \
    pstate->position += count;                                                 \
    if (count < sizeof(buffer))                                                \
    {                                                                          \
        pstate->error_msg = eof_or_error_msg(pstate->stream);                  \
    }

// Macros to define parse_<endian>_<type> functions

#define define_parse_endian_bool(endian, bits)                                 \
    void parse_##endian##_bool##bits(bool *number, int64_t true_rep,           \
                                     uint32_t false_rep, PState *pstate)       \
    {                                                                          \
        if (!pstate->error_msg)                                                \
        {                                                                      \
            union                                                              \
            {                                                                  \
                char           c_val[sizeof(uint##bits##_t)];                  \
                uint##bits##_t i_val;                                          \
            } buffer;                                                          \
                                                                               \
            read_stream_update_position;                                       \
            buffer.i_val = endian##bits##toh(buffer.i_val);                    \
            if (true_rep < 0)                                                  \
            {                                                                  \
                *number = (buffer.i_val != false_rep);                         \
            }                                                                  \
            else if (buffer.i_val == (uint32_t)true_rep)                       \
            {                                                                  \
                *number = true;                                                \
            }                                                                  \
            else if (buffer.i_val == false_rep)                                \
            {                                                                  \
                *number = false;                                               \
            }                                                                  \
            else                                                               \
            {                                                                  \
                pstate->error_msg = "Unable to parse boolean";                 \
            }                                                                  \
        }                                                                      \
    }

#define define_parse_endian_real(endian, type, bits)                           \
    void parse_##endian##_##type(type *number, PState *pstate)                 \
    {                                                                          \
        if (!pstate->error_msg)                                                \
        {                                                                      \
            union                                                              \
            {                                                                  \
                char           c_val[sizeof(type)];                            \
                type           f_val;                                          \
                uint##bits##_t i_val;                                          \
            } buffer;                                                          \
                                                                               \
            read_stream_update_position;                                       \
            buffer.i_val = endian##bits##toh(buffer.i_val);                    \
            *number = buffer.f_val;                                            \
        }                                                                      \
    }

#define define_parse_endian_integer(endian, type, bits)                        \
    void parse_##endian##_##type##bits(type##bits##_t *number, PState *pstate) \
    {                                                                          \
        if (!pstate->error_msg)                                                \
        {                                                                      \
            union                                                              \
            {                                                                  \
                char           c_val[sizeof(type##bits##_t)];                  \
                type##bits##_t i_val;                                          \
            } buffer;                                                          \
                                                                               \
            read_stream_update_position;                                       \
            *number = endian##bits##toh(buffer.i_val);                         \
        }                                                                      \
    }

// Define functions to parse binary real numbers and integers

define_parse_endian_bool(be, 16);
define_parse_endian_bool(be, 32);
define_parse_endian_bool(be, 8);

define_parse_endian_real(be, double, 64)
define_parse_endian_real(be, float, 32)

define_parse_endian_integer(be, int, 16)
define_parse_endian_integer(be, int, 32)
define_parse_endian_integer(be, int, 64)
define_parse_endian_integer(be, int, 8)

define_parse_endian_integer(be, uint, 16)
define_parse_endian_integer(be, uint, 32)
define_parse_endian_integer(be, uint, 64)
define_parse_endian_integer(be, uint, 8)

define_parse_endian_bool(le, 16);
define_parse_endian_bool(le, 32);
define_parse_endian_bool(le, 8);

define_parse_endian_real(le, double, 64)
define_parse_endian_real(le, float, 32)

define_parse_endian_integer(le, int, 16)
define_parse_endian_integer(le, int, 32)
define_parse_endian_integer(le, int, 64)
define_parse_endian_integer(le, int, 8)

define_parse_endian_integer(le, uint, 16)
define_parse_endian_integer(le, uint, 32)
define_parse_endian_integer(le, uint, 64)
define_parse_endian_integer(le, uint, 8)

// Define function to parse fill bytes until end position is reached

void
parse_fill_bytes(size_t end_position, PState *pstate)
{
    while (!pstate->error_msg && pstate->position < end_position)
    {
        char   buffer;
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);

        pstate->position += count;
        if (count < sizeof(buffer))
        {
            pstate->error_msg = eof_or_error_msg(pstate->stream);
        }
    }
}

// Define function to validate number is same as fixed value after parse

void
parse_validate_fixed(bool same, const char *element, PState *pstate)
{
    UNUSED(element); // because managing strings hard in embedded C
    if (!pstate->error_msg && !same)
    {
        // Error message would be easier to assemble and
        // internationalize if we used an error struct with multiple
        // fields instead of a const char string.
        pstate->error_msg = "Parse: Value of element does not match value of "
                            "its 'fixed' attribute";
    }
}
