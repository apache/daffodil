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

typedef struct foo
{
    InfosetBase _base;
    int32_t     a;
    int32_t     b;
    int32_t     c;
} foo;

typedef struct bar
{
    InfosetBase _base;
    double      x;
    double      y;
    double      z;
} bar;

typedef struct data
{
    InfosetBase _base;
    size_t      _choice; // choice of which union field to use
    union
    {
        foo foo;
        bar bar;
    };
} data;

typedef struct NestedUnion
{
    InfosetBase _base;
    int32_t     tag;
    data data;
} NestedUnion;

#endif // GENERATED_CODE_H
