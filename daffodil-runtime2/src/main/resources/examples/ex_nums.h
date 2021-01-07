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
#include <stdint.h>   // for int16_t, int32_t, int64_t, int8_t, uint16_t, uint32_t, uint64_t, uint8_t

// Define some infoset structures

typedef struct array
{
    InfosetBase _base;
    int16_t     be_int16[3];
    float       be_float[3];
} array;

typedef struct bigEndian
{
    InfosetBase _base;
    double      be_double;
    float       be_float;
    uint64_t    be_uint64;
    uint32_t    be_uint32;
    uint16_t    be_uint16;
    uint8_t     be_uint8;
    int64_t     be_int64;
    int32_t     be_int32;
    int16_t     be_int16;
    int8_t      be_int8;
} bigEndian;

typedef struct littleEndian
{
    InfosetBase _base;
    uint64_t    le_uint64;
    uint32_t    le_uint32;
    uint16_t    le_uint16;
    uint8_t     le_uint8;
    int64_t     le_int64;
    int32_t     le_int32;
    int16_t     le_int16;
    int8_t      le_int8;
    float       le_float;
    double      le_double;
} littleEndian;

typedef struct ex_nums
{
    InfosetBase _base;
    array array;
    bigEndian bigEndian;
    littleEndian littleEndian;
} ex_nums;

#endif // GENERATED_CODE_H
