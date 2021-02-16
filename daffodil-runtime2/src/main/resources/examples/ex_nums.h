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

#ifndef GENERATED_CODE_H
#define GENERATED_CODE_H

#include "infoset.h"  // for InfosetBase
#include <stdbool.h>  // for bool
#include <stdint.h>   // for int16_t, int32_t, int64_t, int8_t, uint16_t, uint32_t, uint64_t, uint8_t

// Define infoset structures

typedef struct array
{
    InfosetBase _base;
    bool        be_bool16[2];
    float       be_float[3];
    int16_t     be_int16[3];
} array;

typedef struct bigEndian
{
    InfosetBase _base;
    bool        be_bool16;
    bool        be_bool32;
    bool        be_bool8;
    bool        be_boolean;
    double      be_double;
    float       be_float;
    int16_t     be_int16;
    int32_t     be_int32;
    int64_t     be_int64;
    int8_t      be_int8;
    int16_t     be_integer16;
    uint16_t    be_uint16;
    uint32_t    be_uint32;
    uint64_t    be_uint64;
    uint8_t     be_uint8;
    uint32_t    be_nonNegativeInteger32;
} bigEndian;

typedef struct littleEndian
{
    InfosetBase _base;
    bool        le_bool16;
    bool        le_bool32;
    bool        le_bool8;
    bool        le_boolean;
    double      le_double;
    float       le_float;
    int16_t     le_int16;
    int32_t     le_int32;
    int64_t     le_int64;
    int8_t      le_int8;
    int64_t     le_integer64;
    uint16_t    le_uint16;
    uint32_t    le_uint32;
    uint64_t    le_uint64;
    uint8_t     le_uint8;
    uint8_t     le_nonNegativeInteger8;
} littleEndian;

typedef struct fixed
{
    InfosetBase _base;
    bool        boolean_false;
    bool        boolean_true;
    float       float_1_5;
    int32_t     int_32;
} fixed;

typedef struct ex_nums
{
    InfosetBase _base;
    array array;
    bigEndian bigEndian;
    littleEndian littleEndian;
    fixed fixed;
} ex_nums;

#endif // GENERATED_CODE_H
