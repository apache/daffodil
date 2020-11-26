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

#include "infoset.h" // for InfosetBase
#include <stdint.h>  // for int32_t

// Define some infoset structures

typedef struct c2
{
    InfosetBase _base;
    int32_t     e2;
    int32_t     e3;
} c2;

typedef struct c1
{
    InfosetBase _base;
    int32_t     e1;
    c2          c2;
} c1;

#endif // GENERATED_CODE_H
