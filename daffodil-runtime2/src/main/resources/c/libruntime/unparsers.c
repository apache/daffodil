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

#include "unparsers.h"
#include <endian.h>   // for htobe32, htobe64, htole32, htole64, htobe16, htole16
#include <stdbool.h>  // for bool
#include <stdio.h>    // for fwrite, size_t

// Macros that are not defined by <endian.h>

#define htobe8(var) var

#define htole8(var) var

// Helper macro to reduce duplication of C code writing stream,
// updating position, and checking for errors

#define write_stream_update_position                                           \
    size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);    \
    ustate->position += count;                                                 \
    if (count < sizeof(buffer))                                                \
    {                                                                          \
        ustate->error_msg = eof_or_error_msg(ustate->stream);                  \
    }

// Macros to define unparse_<endian>_<type> functions

#define define_unparse_endian_bool(endian, bits)                               \
    void unparse_##endian##_bool##bits(bool number, uint32_t true_rep,         \
                                       uint32_t false_rep, UState *ustate)     \
    {                                                                          \
        if (!ustate->error_msg)                                                \
        {                                                                      \
            union                                                              \
            {                                                                  \
                char           c_val[sizeof(uint##bits##_t)];                  \
                uint##bits##_t i_val;                                          \
            } buffer;                                                          \
                                                                               \
            buffer.i_val = hto##endian##bits(number ? true_rep : false_rep);   \
            write_stream_update_position;                                      \
        }                                                                      \
    }

#define define_unparse_endian_real(endian, type, bits)                         \
    void unparse_##endian##_##type(type number, UState *ustate)                \
    {                                                                          \
        if (!ustate->error_msg)                                                \
        {                                                                      \
            union                                                              \
            {                                                                  \
                char           c_val[sizeof(type)];                            \
                type           f_val;                                          \
                uint##bits##_t i_val;                                          \
            } buffer;                                                          \
                                                                               \
            buffer.f_val = number;                                             \
            buffer.i_val = hto##endian##bits(buffer.i_val);                    \
            write_stream_update_position;                                      \
        }                                                                      \
    }

#define define_unparse_endian_integer(endian, type, bits)                      \
    void unparse_##endian##_##type##bits(type##bits##_t number,                \
                                         UState *       ustate)                \
    {                                                                          \
        if (!ustate->error_msg)                                                \
        {                                                                      \
            union                                                              \
            {                                                                  \
                char           c_val[sizeof(type##bits##_t)];                  \
                type##bits##_t i_val;                                          \
            } buffer;                                                          \
                                                                               \
            buffer.i_val = hto##endian##bits(number);                          \
            write_stream_update_position;                                      \
        }                                                                      \
    }

// Define functions to unparse binary real numbers and integers

define_unparse_endian_bool(be, 16);
define_unparse_endian_bool(be, 32);
define_unparse_endian_bool(be, 8);

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

define_unparse_endian_bool(le, 16);
define_unparse_endian_bool(le, 32);
define_unparse_endian_bool(le, 8);

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

// Define function to unparse fill bytes until end position is reached

void
unparse_fill_bytes(size_t end_position, const char fill_byte, UState *ustate)
{
    while (!ustate->error_msg && ustate->position < end_position)
    {
        size_t count = fwrite(&fill_byte, 1, sizeof(fill_byte), ustate->stream);

        ustate->position += count;
        if (count < sizeof(fill_byte))
        {
            ustate->error_msg = eof_or_error_msg(ustate->stream);
        }
    }
}

// Define function to validate number is same as fixed value during unparse

void
unparse_validate_fixed(bool same, const char *element, UState *ustate)
{
    if (!ustate->error_msg && !same)
    {
        // Error message would be easier to assemble and
        // internationalize if we used an error struct with multiple
        // fields instead of a const char string.
        ustate->error_msg = "Unparse: Value of element does not match value of "
                            "its 'fixed' attribute";
        UNUSED(element); // unused because managing strings hard in embedded C
    }
}
