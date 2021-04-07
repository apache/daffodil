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
#include <endian.h>   // for be32toh, le32toh, be16toh, be64toh, le16toh, le64toh
#include <stdbool.h>  // for bool, false, true
#include <stdio.h>    // for fread
#include "errors.h"   // for PState, eof_or_error, Error, ERR_PARSE_BOOL, Error::(anonymous), Diagnostics, need_diagnostics, ERR_FIXED_VALUE

// Macros not defined by <endian.h> which we need for uniformity

#define be8toh(var) var
#define le8toh(var) var

// Helper macro to reduce duplication of C code reading stream,
// updating position, and checking for errors

#define read_stream_update_position                                            \
    size_t count = fread(&buffer.c_val, 1, sizeof(buffer), pstate->stream);    \
    pstate->position += count;                                                 \
    if (count < sizeof(buffer))                                                \
    {                                                                          \
        pstate->error = eof_or_error(pstate->stream);                          \
        if (pstate->error) return;                                             \
    }

// Macros to define parse_<endian>_<type> functions

#define define_parse_endian_bool(endian, bits)                                 \
    void parse_##endian##_bool##bits(bool *number, int64_t true_rep,           \
                                     uint32_t false_rep, PState *pstate)       \
    {                                                                          \
        union                                                                  \
        {                                                                      \
            char           c_val[sizeof(uint##bits##_t)];                      \
            uint##bits##_t i_val;                                              \
        } buffer;                                                              \
                                                                               \
        read_stream_update_position;                                           \
        buffer.i_val = endian##bits##toh(buffer.i_val);                        \
        if (true_rep < 0)                                                      \
        {                                                                      \
            *number = (buffer.i_val != false_rep);                             \
        }                                                                      \
        else if (buffer.i_val == (uint32_t)true_rep)                           \
        {                                                                      \
            *number = true;                                                    \
        }                                                                      \
        else if (buffer.i_val == false_rep)                                    \
        {                                                                      \
            *number = false;                                                   \
        }                                                                      \
        else                                                                   \
        {                                                                      \
            static Error error = {ERR_PARSE_BOOL, {NULL}};                     \
            error.d64 = (int64_t)buffer.i_val;                                 \
            pstate->error = &error;                                            \
        }                                                                      \
    }

#define define_parse_endian_real(endian, type, bits)                           \
    void parse_##endian##_##type(type *number, PState *pstate)                 \
    {                                                                          \
        union                                                                  \
        {                                                                      \
            char           c_val[sizeof(type)];                                \
            type           f_val;                                              \
            uint##bits##_t i_val;                                              \
        } buffer;                                                              \
                                                                               \
        read_stream_update_position;                                           \
        buffer.i_val = endian##bits##toh(buffer.i_val);                        \
        *number = buffer.f_val;                                                \
    }

#define define_parse_endian_integer(endian, type, bits)                        \
    void parse_##endian##_##type##bits(type##bits##_t *number, PState *pstate) \
    {                                                                          \
        union                                                                  \
        {                                                                      \
            char           c_val[sizeof(type##bits##_t)];                      \
            type##bits##_t i_val;                                              \
        } buffer;                                                              \
                                                                               \
        read_stream_update_position;                                           \
        *number = endian##bits##toh(buffer.i_val);                             \
    }

// Parse binary booleans, real numbers, and integers

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

// Parse fill bytes until end position is reached

void
parse_fill_bytes(size_t end_position, PState *pstate)
{
    union
    {
        char c_val[1];
    } buffer;

    while (pstate->position < end_position)
    {
        read_stream_update_position;
    }
}

// Validate parsed number is same as fixed value

void
parse_validate_fixed(bool same, const char *element, PState *pstate)
{
    if (!same)
    {
        Diagnostics *validati = need_diagnostics();
        pstate->validati = validati;

        if (validati->length <
            sizeof(validati->array) / sizeof(*validati->array))
        {
            Error *error = &validati->array[validati->length++];
            error->code = ERR_FIXED_VALUE;
            error->s = element;
        }
    }
}
