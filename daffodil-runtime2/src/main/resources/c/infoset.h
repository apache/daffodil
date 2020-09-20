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

#ifndef INFOSET_H
#define INFOSET_H

#include <stddef.h> // for ptrdiff_t
#include <stdint.h> // for int32_t
#include <stdio.h>  // for FILE, size_t

// Prototypes needed for compilation

struct ElementRuntimeData;
struct InfosetBase;
struct PState;
struct UState;
struct VisitEventHandler;

typedef struct ElementRuntimeData ERD;
typedef struct InfosetBase        InfosetBase;
typedef struct PState             PState;
typedef struct UState             UState;
typedef struct VisitEventHandler  VisitEventHandler;

typedef void (*ERDInitSelf)(InfosetBase *infoNode);
typedef const char *(*ERDParseSelf)(InfosetBase * infoNode,
                                    const PState *pstate);
typedef const char *(*ERDUnparseSelf)(const InfosetBase *infoNode,
                                      const UState *     ustate);

typedef const char *(*VisitStartDocument)(const VisitEventHandler *handler);
typedef const char *(*VisitEndDocument)(const VisitEventHandler *handler);
typedef const char *(*VisitStartComplex)(const VisitEventHandler *handler,
                                         const InfosetBase *      base);
typedef const char *(*VisitEndComplex)(const VisitEventHandler *handler,
                                       const InfosetBase *      base);
typedef const char *(*VisitInt32Elem)(const VisitEventHandler *handler,
                                      const ERD *erd, const int32_t *location);

// NamedQName - name of an infoset element

typedef struct NamedQName
{
    const char *prefix; // prefix (optional, may be NULL)
    const char *local;  // local name
    const char *ns;     // namespace URI (optional, may be NULL)
} NamedQName;

// TypeCode - type of an infoset element

enum TypeCode
{
    COMPLEX,
    PRIMITIVE_INT32
};

// ERD - element runtime data needed to parse/unparse objects

typedef struct ElementRuntimeData
{
    const NamedQName    namedQName;
    const enum TypeCode typeCode;
    const size_t        numChildren;
    const ptrdiff_t *   offsets;
    const ERD **        childrenERDs;

    const ERDInitSelf    initSelf;
    const ERDParseSelf   parseSelf;
    const ERDUnparseSelf unparseSelf;
} ERD;

// InfosetBase - representation of an infoset element

typedef struct InfosetBase
{
    const ERD *erd;
} InfosetBase;

// PState - parser state while parsing input

typedef struct PState
{
    FILE *stream; // input to read from
} PState;

// UState - unparser state while unparsing infoset

typedef struct UState
{
    FILE *stream; // output to write to
} UState;

// VisitEventHandler - methods to be called when walking an infoset

typedef struct VisitEventHandler
{
    const VisitStartDocument visitStartDocument;
    const VisitEndDocument   visitEndDocument;
    const VisitStartComplex  visitStartComplex;
    const VisitEndComplex    visitEndComplex;
    const VisitInt32Elem     visitInt32Elem;
} VisitEventHandler;

// get_erd_name, get_erd_xmlns, get_erd_ns - get name and xmlns
// attribute/value from ERD to use for XML element

extern const char *get_erd_name(const ERD *erd);
extern const char *get_erd_xmlns(const ERD *erd);
extern const char *get_erd_ns(const ERD *erd);

// rootElement - return a root element to walk while parsing or unparsing

// (actual definition will be in generated_code.c, not infoset.c)
extern InfosetBase *rootElement();

// walkInfoset - walk an infoset and call VisitEventHandler methods

extern const char *walkInfoset(const VisitEventHandler *handler,
                               const InfosetBase *      infoset);

// eof_or_error_msg - check if a stream has its eof or error indicator set

extern const char *eof_or_error_msg(FILE *stream);

#endif // INFOSET_H
