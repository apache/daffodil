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
#include <endian.h>  // for be32toh, be64toh, le32toh, le64toh, be16toh, le16toh
#include <stdio.h>   // for fread, size_t

// Macros that are not defined by <endian.h>

#define be8toh(var) var

#define le8toh(var) var

// Macros to define parse_<endian>_<type> functions

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
            size_t count =                                                     \
                fread(&buffer.c_val, 1, sizeof(buffer), pstate->stream);       \
            if (count < sizeof(buffer))                                        \
            {                                                                  \
                pstate->error_msg = eof_or_error_msg(pstate->stream);          \
            }                                                                  \
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
            size_t count =                                                     \
                fread(&buffer.c_val, 1, sizeof(buffer), pstate->stream);       \
            if (count < sizeof(buffer))                                        \
            {                                                                  \
                pstate->error_msg = eof_or_error_msg(pstate->stream);          \
            }                                                                  \
            *number = endian##bits##toh(buffer.i_val);                         \
        }                                                                      \
    }

// Define functions to parse binary real and integer numbers

define_parse_endian_real(be, double, 64)

define_parse_endian_real(be, float, 32)

define_parse_endian_integer(be, uint, 64)

define_parse_endian_integer(be, uint, 32)

define_parse_endian_integer(be, uint, 16)

define_parse_endian_integer(be, uint, 8)

define_parse_endian_integer(be, int, 64)

define_parse_endian_integer(be, int, 32)

define_parse_endian_integer(be, int, 16)

define_parse_endian_integer(be, int, 8)

define_parse_endian_real(le, double, 64)

define_parse_endian_real(le, float, 32)

define_parse_endian_integer(le, uint, 64)

define_parse_endian_integer(le, uint, 32)

define_parse_endian_integer(le, uint, 16)

define_parse_endian_integer(le, uint, 8)

define_parse_endian_integer(le, int, 64)

define_parse_endian_integer(le, int, 32)

define_parse_endian_integer(le, int, 16)

define_parse_endian_integer(le, int, 8)
