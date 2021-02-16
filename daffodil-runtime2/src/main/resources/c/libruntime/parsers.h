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

#include "infoset.h"  // for PState
#include <stdbool.h>  // for bool
#include <stdint.h>   // for int16_t, int32_t, int64_t, int8_t, uint16_t, uint32_t, uint64_t, uint8_t

// Functions to parse binary booleans, real numbers, and integers

extern void parse_be_bool16(bool *number, int64_t true_rep, uint32_t false_rep,
                            PState *pstate);
extern void parse_be_bool32(bool *number, int64_t true_rep, uint32_t false_rep,
                            PState *pstate);
extern void parse_be_bool8(bool *number, int64_t true_rep, uint32_t false_rep,
                           PState *pstate);

extern void parse_be_double(double *number, PState *pstate);
extern void parse_be_float(float *number, PState *pstate);

extern void parse_be_int16(int16_t *number, PState *pstate);
extern void parse_be_int32(int32_t *number, PState *pstate);
extern void parse_be_int64(int64_t *number, PState *pstate);
extern void parse_be_int8(int8_t *number, PState *pstate);

extern void parse_be_uint16(uint16_t *number, PState *pstate);
extern void parse_be_uint32(uint32_t *number, PState *pstate);
extern void parse_be_uint64(uint64_t *number, PState *pstate);
extern void parse_be_uint8(uint8_t *number, PState *pstate);

extern void parse_le_bool16(bool *number, int64_t true_rep, uint32_t false_rep,
                            PState *pstate);
extern void parse_le_bool32(bool *number, int64_t true_rep, uint32_t false_rep,
                            PState *pstate);
extern void parse_le_bool8(bool *number, int64_t true_rep, uint32_t false_rep,
                           PState *pstate);

extern void parse_le_double(double *number, PState *pstate);
extern void parse_le_float(float *number, PState *pstate);

extern void parse_le_int16(int16_t *number, PState *pstate);
extern void parse_le_int32(int32_t *number, PState *pstate);
extern void parse_le_int64(int64_t *number, PState *pstate);
extern void parse_le_int8(int8_t *number, PState *pstate);

extern void parse_le_uint16(uint16_t *number, PState *pstate);
extern void parse_le_uint32(uint32_t *number, PState *pstate);
extern void parse_le_uint64(uint64_t *number, PState *pstate);
extern void parse_le_uint8(uint8_t *number, PState *pstate);

// Function to parse fill bytes until end position is reached

extern void parse_fill_bytes(size_t end_position, PState *pstate);

// Function to validate number is same as fixed value after parse

extern void parse_validate_fixed(bool same, const char *element,
                                 PState *pstate);

#endif // PARSERS_H
