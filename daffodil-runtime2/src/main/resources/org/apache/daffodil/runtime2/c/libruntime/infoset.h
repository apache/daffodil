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

// clang-format off
#include <stdbool.h>  // for bool
#include <stddef.h>   // for size_t
#include <stdint.h>   // for uint8_t
#include <stdio.h>    // for FILE
#include "errors.h"   // for Error, Diagnostics
// clang-format on

// Prototypes needed for compilation

struct ERD;
struct InfosetBase;
struct PState;
struct UState;
struct VisitEventHandler;

typedef void (*ERDParseSelf)(struct InfosetBase *infoNode, struct PState *pstate);
typedef void (*ERDUnparseSelf)(const struct InfosetBase *infoNode, struct UState *ustate);
typedef const Error *(*InitChoiceRD)(const struct InfosetBase *infoNode,
                                     const struct InfosetBase *rootElement);

typedef const Error *(*VisitStartDocument)(const struct VisitEventHandler *handler);
typedef const Error *(*VisitEndDocument)(const struct VisitEventHandler *handler);
typedef const Error *(*VisitStartComplex)(const struct VisitEventHandler *handler,
                                          const struct InfosetBase *      base);
typedef const Error *(*VisitEndComplex)(const struct VisitEventHandler *handler,
                                        const struct InfosetBase *      base);
typedef const Error *(*VisitSimpleElem)(const struct VisitEventHandler *handler, const struct ERD *erd,
                                        const void *number);

// NamedQName - name of an infoset element

typedef struct NamedQName
{
    const char *prefix; // prefix (optional, may be NULL)
    const char *local;  // local name
    const char *ns;     // namespace URI (optional, may be NULL)
} NamedQName;

// TypeCode - types of infoset elements

enum TypeCode
{
    CHOICE,
    COMPLEX,
    PRIMITIVE_BOOLEAN,
    PRIMITIVE_DOUBLE,
    PRIMITIVE_FLOAT,
    PRIMITIVE_HEXBINARY,
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

typedef struct ERD
{
    const NamedQName    namedQName;
    const enum TypeCode typeCode;
    const size_t        numChildren;
    const size_t *      offsets;
    const struct ERD ** childrenERDs;

    const ERDParseSelf   parseSelf;
    const ERDUnparseSelf unparseSelf;
    const InitChoiceRD   initChoice;
} ERD;

// HexBinary - data of a hexBinary element

typedef struct HexBinary
{
    uint8_t *array;         // pointer to data in byte array
    size_t   lengthInBytes; // length of data in bytes
    bool     dynamic;       // true if byte array was malloc'ed
} HexBinary;

// InfosetBase - metadata of an infoset element

typedef struct InfosetBase
{
    const ERD *erd;
} InfosetBase;

// PState - mutable state while parsing data

typedef struct PState
{
    FILE *       stream;      // stream to read data from (input)
    size_t       bitPos0b;    // 0-based position of last successful parse (1-bit granularity)
    Diagnostics *diagnostics; // any validation diagnostics
    const Error *error;       // any error which stops parse
    uint8_t      unreadBits;  // any buffered bits not read yet
    uint8_t      unreadLen;   // number of buffered bits not read yet
} PState;

// UState - mutable state while unparsing infoset

typedef struct UState
{
    FILE *       stream;      // stream to write data to (output)
    size_t       bitPos0b;    // 0-based position of last successful write (1-bit granularity)
    Diagnostics *diagnostics; // any validation diagnostics
    const Error *error;       // any error which stops unparse
    uint8_t      unwritBits;  // any buffered bits not written yet
    uint8_t      unwritLen;   // number of buffered bits not written yet
} UState;

// VisitEventHandler - methods to be called when walking an infoset

typedef struct VisitEventHandler
{
    const VisitStartDocument visitStartDocument;
    const VisitEndDocument   visitEndDocument;
    const VisitStartComplex  visitStartComplex;
    const VisitEndComplex    visitEndComplex;
    const VisitSimpleElem    visitSimpleElem;
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

extern const Error *walkInfoset(const VisitEventHandler *handler, const InfosetBase *infoset);

// flushUState - flush any buffered bits not written yet

extern void flushUState(UState *ustate);

#endif // INFOSET_H
