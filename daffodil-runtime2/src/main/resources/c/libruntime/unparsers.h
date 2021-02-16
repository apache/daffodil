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

#ifndef UNPARSERS_H
#define UNPARSERS_H

#include "infoset.h"  // for UState
#include <stdbool.h>  // for bool
#include <stdint.h>   // for int16_t, int32_t, int64_t, int8_t, uint16_t, uint32_t, uint64_t, uint8_t

// Functions to unparse binary booleans, real numbers, and integers

extern void unparse_be_bool16(bool number, uint32_t true_rep,
                              uint32_t false_rep, UState *ustate);
extern void unparse_be_bool32(bool number, uint32_t true_rep,
                              uint32_t false_rep, UState *ustate);
extern void unparse_be_bool8(bool number, uint32_t true_rep, uint32_t false_rep,
                             UState *ustate);

extern void unparse_be_double(double number, UState *ustate);
extern void unparse_be_float(float number, UState *ustate);

extern void unparse_be_int16(int16_t number, UState *ustate);
extern void unparse_be_int32(int32_t number, UState *ustate);
extern void unparse_be_int64(int64_t number, UState *ustate);
extern void unparse_be_int8(int8_t number, UState *ustate);

extern void unparse_be_uint16(uint16_t number, UState *ustate);
extern void unparse_be_uint32(uint32_t number, UState *ustate);
extern void unparse_be_uint64(uint64_t number, UState *ustate);
extern void unparse_be_uint8(uint8_t number, UState *ustate);

extern void unparse_le_bool16(bool number, uint32_t true_rep,
                              uint32_t false_rep, UState *ustate);
extern void unparse_le_bool32(bool number, uint32_t true_rep,
                              uint32_t false_rep, UState *ustate);
extern void unparse_le_bool8(bool number, uint32_t true_rep, uint32_t false_rep,
                             UState *ustate);

extern void unparse_le_double(double number, UState *ustate);
extern void unparse_le_float(float number, UState *ustate);

extern void unparse_le_int16(int16_t number, UState *ustate);
extern void unparse_le_int32(int32_t number, UState *ustate);
extern void unparse_le_int64(int64_t number, UState *ustate);
extern void unparse_le_int8(int8_t number, UState *ustate);

extern void unparse_le_uint16(uint16_t number, UState *ustate);
extern void unparse_le_uint32(uint32_t number, UState *ustate);
extern void unparse_le_uint64(uint64_t number, UState *ustate);
extern void unparse_le_uint8(uint8_t number, UState *ustate);

// Function to unparse fill bytes until end position is reached

extern void unparse_fill_bytes(size_t end_position, const char fill_byte,
                               UState *ustate);

// Function to validate number is same as fixed value during unparse

extern void unparse_validate_fixed(bool same, const char *element,
                                   UState *ustate);

#endif // UNPARSERS_H
