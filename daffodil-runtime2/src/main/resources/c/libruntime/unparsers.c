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
#include <endian.h>  // for htobe32, htobe64, htole32, htole64, htobe16, htole16
#include <stdio.h>   // for fwrite, size_t

// Macros to expand into functions below

#define define_unparse_endian_real(endian, type, bits)                         \
    extern void unparse_##endian##_##type(type number, UState *ustate)         \
    {                                                                          \
        if (!ustate->error_msg)                                                \
        {                                                                      \
            union                                                              \
            {                                                                  \
                char           c_val[sizeof(type)];                            \
                type           f_val;                                          \
                uint##bits##_t i_val;                                          \
            } buffer;                                                          \
            buffer.f_val = number;                                             \
            buffer.i_val = hto##endian##bits(buffer.i_val);                    \
            size_t count =                                                     \
                fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);       \
            if (count < sizeof(buffer))                                        \
            {                                                                  \
                ustate->error_msg = eof_or_error_msg(ustate->stream);          \
            }                                                                  \
        }                                                                      \
    }

#define define_unparse_endian_integer(endian, type, bits)                      \
    extern void unparse_##endian##_##type##bits(type##bits##_t number,         \
                                                UState *       ustate)         \
    {                                                                          \
        if (!ustate->error_msg)                                                \
        {                                                                      \
            union                                                              \
            {                                                                  \
                char           c_val[sizeof(type##bits##_t)];                  \
                type##bits##_t i_val;                                          \
            } buffer;                                                          \
            buffer.i_val = hto##endian##bits(number);                          \
            size_t count =                                                     \
                fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);       \
            if (count < sizeof(buffer))                                        \
            {                                                                  \
                ustate->error_msg = eof_or_error_msg(ustate->stream);          \
            }                                                                  \
        }                                                                      \
    }

#define htobe8(var) var

#define htole8(var) var

// Define functions to unparse binary real and integer numbers

define_unparse_endian_real(be, double, 64)

define_unparse_endian_real(be, float, 32)

define_unparse_endian_integer(be, uint, 64)

define_unparse_endian_integer(be, uint, 32)

define_unparse_endian_integer(be, uint, 16)

define_unparse_endian_integer(be, uint, 8)

define_unparse_endian_integer(be, int, 64)

define_unparse_endian_integer(be, int, 32)

define_unparse_endian_integer(be, int, 16)

define_unparse_endian_integer(be, int, 8)

define_unparse_endian_real(le, double, 64)

define_unparse_endian_real(le, float, 32)

define_unparse_endian_integer(le, uint, 64)

define_unparse_endian_integer(le, uint, 32)

define_unparse_endian_integer(le, uint, 16)

define_unparse_endian_integer(le, uint, 8)

define_unparse_endian_integer(le, int, 64)

define_unparse_endian_integer(le, int, 32)

define_unparse_endian_integer(le, int, 16)

define_unparse_endian_integer(le, int, 8)
