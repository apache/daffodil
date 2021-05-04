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
#include "unparsers.h"
#include <endian.h>   // for htobe32, htole32, htobe16, htobe64, htole16, htole64
#include <stdbool.h>  // for bool
#include <stdio.h>    // for fwrite
#include "errors.h"   // for eof_or_error, add_diagnostic, get_diagnostics, ERR_FIXED_VALUE, Diagnostics, Error
// clang-format on

// Macros not defined by <endian.h> which we need for uniformity

#define htobe8(var) var
#define htole8(var) var

// Helper macro to reduce duplication of C code writing stream,
// updating position, and checking for errors

#define write_stream_update_position                                                                         \
    size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);                                  \
    ustate->position += count;                                                                               \
    if (count < sizeof(buffer))                                                                              \
    {                                                                                                        \
        ustate->error = eof_or_error(ustate->stream);                                                        \
        if (ustate->error) return;                                                                           \
    }

// Macros to define unparse_<endian>_<type> functions

#define define_unparse_endian_bool(endian, bits)                                                             \
    void unparse_##endian##_bool##bits(bool number, uint32_t true_rep, uint32_t false_rep, UState *ustate)   \
    {                                                                                                        \
        union                                                                                                \
        {                                                                                                    \
            char           c_val[sizeof(uint##bits##_t)];                                                    \
            uint##bits##_t i_val;                                                                            \
        } buffer;                                                                                            \
                                                                                                             \
        buffer.i_val = hto##endian##bits(number ? true_rep : false_rep);                                     \
        write_stream_update_position;                                                                        \
    }

#define define_unparse_endian_real(endian, type, bits)                                                       \
    void unparse_##endian##_##type(type number, UState *ustate)                                              \
    {                                                                                                        \
        union                                                                                                \
        {                                                                                                    \
            char           c_val[sizeof(type)];                                                              \
            type           f_val;                                                                            \
            uint##bits##_t i_val;                                                                            \
        } buffer;                                                                                            \
                                                                                                             \
        buffer.f_val = number;                                                                               \
        buffer.i_val = hto##endian##bits(buffer.i_val);                                                      \
        write_stream_update_position;                                                                        \
    }

#define define_unparse_endian_integer(endian, type, bits)                                                    \
    void unparse_##endian##_##type##bits(type##bits##_t number, UState *ustate)                              \
    {                                                                                                        \
        union                                                                                                \
        {                                                                                                    \
            char           c_val[sizeof(type##bits##_t)];                                                    \
            type##bits##_t i_val;                                                                            \
        } buffer;                                                                                            \
                                                                                                             \
        buffer.i_val = hto##endian##bits(number);                                                            \
        write_stream_update_position;                                                                        \
    }

// Unparse binary booleans, real numbers, and integers

define_unparse_endian_bool(be, 16)
define_unparse_endian_bool(be, 32)
define_unparse_endian_bool(be, 8)

define_unparse_endian_real(be, double, 64)
define_unparse_endian_real(be, float, 32)

define_unparse_endian_integer(be, int, 16)
define_unparse_endian_integer(be, int, 32)
define_unparse_endian_integer(be, int, 64)
define_unparse_endian_integer(be, int, 8)

define_unparse_endian_integer(be, uint, 16)
define_unparse_endian_integer(be, uint, 32)
define_unparse_endian_integer(be, uint, 64)
define_unparse_endian_integer(be, uint, 8)

define_unparse_endian_bool(le, 16)
define_unparse_endian_bool(le, 32)
define_unparse_endian_bool(le, 8)

define_unparse_endian_real(le, double, 64)
define_unparse_endian_real(le, float, 32)

define_unparse_endian_integer(le, int, 16)
define_unparse_endian_integer(le, int, 32)
define_unparse_endian_integer(le, int, 64)
define_unparse_endian_integer(le, int, 8)

define_unparse_endian_integer(le, uint, 16)
define_unparse_endian_integer(le, uint, 32)
define_unparse_endian_integer(le, uint, 64)
define_unparse_endian_integer(le, uint, 8)

// Unparse fill bytes until end position is reached

void
unparse_fill_bytes(size_t end_position, const char fill_byte, UState *ustate)
{
    union
    {
        char c_val[1];
    } buffer;

    buffer.c_val[0] = fill_byte;

    while (ustate->position < end_position)
    {
        write_stream_update_position;
    }
}

// Validate unparsed number is same as fixed value

void
unparse_validate_fixed(bool same, const char *element, UState *ustate)
{
    if (!same)
    {
        Diagnostics *diagnostics = get_diagnostics();
        const Error  error = {ERR_FIXED_VALUE, {.s = element}};

        add_diagnostic(diagnostics, &error);
        ustate->diagnostics = diagnostics;
    }
}
