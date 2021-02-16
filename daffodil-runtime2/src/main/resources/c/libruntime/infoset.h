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

#include <stdbool.h>  // for bool
#include <stddef.h>   // for size_t
#include <stdio.h>    // for FILE

// Prototypes needed for compilation

typedef struct ElementRuntimeData ERD;
typedef struct InfosetBase        InfosetBase;
typedef struct PState             PState;
typedef struct UState             UState;
typedef struct VisitEventHandler  VisitEventHandler;

typedef void (*ERDInitSelf)(InfosetBase *infoNode);
typedef void (*ERDParseSelf)(InfosetBase *infoNode, PState *pstate);
typedef void (*ERDUnparseSelf)(const InfosetBase *infoNode, UState *ustate);
typedef bool (*InitChoiceRD)(const InfosetBase *infoNode,
                             const InfosetBase *rootElement);

typedef const char *(*VisitStartDocument)(const VisitEventHandler *handler);
typedef const char *(*VisitEndDocument)(const VisitEventHandler *handler);
typedef const char *(*VisitStartComplex)(const VisitEventHandler *handler,
                                         const InfosetBase *      base);
typedef const char *(*VisitEndComplex)(const VisitEventHandler *handler,
                                       const InfosetBase *      base);
typedef const char *(*VisitNumberElem)(const VisitEventHandler *handler,
                                       const ERD *erd, const void *number);

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
    CHOICE,
    COMPLEX,
    PRIMITIVE_BOOLEAN,
    PRIMITIVE_DOUBLE,
    PRIMITIVE_FLOAT,
    PRIMITIVE_INT16,
    PRIMITIVE_INT32,
    PRIMITIVE_INT64,
    PRIMITIVE_INT8,
    PRIMITIVE_UINT16,
    PRIMITIVE_UINT32,
    PRIMITIVE_UINT64,
    PRIMITIVE_UINT8
};

// ERD - element runtime data needed to parse/unparse objects

typedef struct ElementRuntimeData
{
    const NamedQName    namedQName;
    const enum TypeCode typeCode;
    const size_t        numChildren;
    const size_t *      offsets;
    const ERD **        childrenERDs;

    const ERDInitSelf    initSelf;
    const ERDParseSelf   parseSelf;
    const ERDUnparseSelf unparseSelf;
    const InitChoiceRD   initChoice;
} ERD;

// InfosetBase - metadata of an infoset element

typedef struct InfosetBase
{
    const ERD *erd;
} InfosetBase;

// PState - mutable state while parsing data

typedef struct PState
{
    FILE *      stream;    // input to read data from
    size_t      position;  // 0-based position in stream
    const char *error_msg; // to stop if an error happens
} PState;

// UState - mutable state while unparsing infoset

typedef struct UState
{
    FILE *      stream;    // output to write data to
    size_t      position;  // 0-based position in stream
    const char *error_msg; // to stop if an error happens
} UState;

// VisitEventHandler - methods to be called when walking an infoset

typedef struct VisitEventHandler
{
    const VisitStartDocument visitStartDocument;
    const VisitEndDocument   visitEndDocument;
    const VisitStartComplex  visitStartComplex;
    const VisitEndComplex    visitEndComplex;
    const VisitNumberElem    visitNumberElem;
} VisitEventHandler;

// get_erd_name, get_erd_xmlns, get_erd_ns - get name and xmlns
// attribute/value from ERD to use for XML element

extern const char *get_erd_name(const ERD *erd);
extern const char *get_erd_xmlns(const ERD *erd);
extern const char *get_erd_ns(const ERD *erd);

// rootElement - return an infoset's root element for parsing,
// walking, or unparsing (implementation actually is generated in
// generated_code.c, not defined in infoset.c)

extern InfosetBase *rootElement(void);

// walkInfoset - walk an infoset and call VisitEventHandler methods

extern const char *walkInfoset(const VisitEventHandler *handler,
                               const InfosetBase *      infoset);

// eof_or_error_msg - check if a stream has its eof or error indicator set

extern const char *eof_or_error_msg(FILE *stream);

// NO_CHOICE - define value stored in uninitialized _choice field

static const size_t NO_CHOICE = (size_t)-1;

// UNUSED - suppress compiler warning about unused variable

#define UNUSED(x) (void)(x)

#endif // INFOSET_H
