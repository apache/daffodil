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

#ifndef PARSERS_H
#define PARSERS_H

// auto-maintained by iwyu
// clang-format off
#include <stdbool.h>  // for bool
#include <stddef.h>   // for size_t
#include <stdint.h>   // for int64_t, uint32_t, int16_t, int32_t, int8_t, uint16_t, uint64_t, uint8_t
#include "infoset.h"  // for PState, HexBinary
// clang-format on

// Parse binary booleans, real numbers, and integers

extern void parse_be_bool(bool *number, size_t num_bits, int64_t true_rep, uint32_t false_rep,
                          PState *pstate);
extern void parse_be_double(double *number, size_t num_bits, PState *pstate);
extern void parse_be_float(float *number, size_t num_bits, PState *pstate);

extern void parse_be_int16(int16_t *number, size_t num_bits, PState *pstate);
extern void parse_be_int32(int32_t *number, size_t num_bits, PState *pstate);
extern void parse_be_int64(int64_t *number, size_t num_bits, PState *pstate);
extern void parse_be_int8(int8_t *number, size_t num_bits, PState *pstate);

extern void parse_be_uint16(uint16_t *number, size_t num_bits, PState *pstate);
extern void parse_be_uint32(uint32_t *number, size_t num_bits, PState *pstate);
extern void parse_be_uint64(uint64_t *number, size_t num_bits, PState *pstate);
extern void parse_be_uint8(uint8_t *number, size_t num_bits, PState *pstate);

extern void parse_le_bool(bool *number, size_t num_bits, int64_t true_rep, uint32_t false_rep,
                          PState *pstate);
extern void parse_le_double(double *number, size_t num_bits, PState *pstate);
extern void parse_le_float(float *number, size_t num_bits, PState *pstate);

extern void parse_le_int16(int16_t *number, size_t num_bits, PState *pstate);
extern void parse_le_int32(int32_t *number, size_t num_bits, PState *pstate);
extern void parse_le_int64(int64_t *number, size_t num_bits, PState *pstate);
extern void parse_le_int8(int8_t *number, size_t num_bits, PState *pstate);

extern void parse_le_uint16(uint16_t *number, size_t num_bits, PState *pstate);
extern void parse_le_uint32(uint32_t *number, size_t num_bits, PState *pstate);
extern void parse_le_uint64(uint64_t *number, size_t num_bits, PState *pstate);
extern void parse_le_uint8(uint8_t *number, size_t num_bits, PState *pstate);

// Allocate memory for hexBinary array

extern void alloc_hexBinary(HexBinary *hexBinary, size_t num_bytes, PState *pstate);

// Parse opaque bytes into hexBinary array

extern void parse_hexBinary(HexBinary *hexBinary, PState *pstate);

// Parse alignment bits up to alignmentInBits or end_bitPos0b

extern void parse_align_to(size_t alignmentInBits, PState *pstate);
extern void parse_alignment_bits(size_t end_bitPos0b, PState *pstate);

// Check for any data left over after end of parse

extern void no_leftover_data(PState *pstate);

#endif // PARSERS_H
