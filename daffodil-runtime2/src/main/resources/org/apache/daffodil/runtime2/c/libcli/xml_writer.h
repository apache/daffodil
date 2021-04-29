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

#ifndef XML_WRITER_H
#define XML_WRITER_H

// clang-format off
#include <stdio.h>    // for FILE
#include "infoset.h"  // for VisitEventHandler
#include "stack.h"    // for stack_t
// clang-format on

// XMLWriter - infoset visitor with methods to output XML

typedef struct XMLWriter
{
    const VisitEventHandler handler;
    FILE *                  stream;
    stack_t                 stack;
} XMLWriter;

// XMLWriter methods to pass to walkInfoset method

extern const VisitEventHandler xmlWriterMethods;

#endif // XML_WRITER_H
