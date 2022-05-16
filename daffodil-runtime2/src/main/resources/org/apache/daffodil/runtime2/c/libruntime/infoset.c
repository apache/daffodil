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

// clang-format off
#include "infoset.h"
#include <stdio.h>   // for fwrite
#include <string.h>  // for memccpy
#include "errors.h"  // for Error, eof_or_error, LIMIT_NAME_LENGTH
// clang-format on

// get_erd_name, get_erd_xmlns, get_erd_ns - get name and xmlns
// attribute/value from ERD to use on XML element

const char *
get_erd_name(const ERD *erd)
{
    static char name[LIMIT_NAME_LENGTH];

    char *next = name;
    char *last = name + sizeof(name) - 1;

    if (erd->namedQName.prefix)
    {
        next = memccpy(next, erd->namedQName.prefix, 0, last - next);
        if (next)
        {
            --next;
        }
    }
    if (next && erd->namedQName.prefix)
    {
        next = memccpy(next, ":", 0, last - next);
        if (next)
        {
            --next;
        }
    }
    if (next)
    {
        next = memccpy(next, erd->namedQName.local, 0, last - next);
        if (next)
        {
            --next;
        }
    }
    if (!next)
    {
        *last = 0;
    }

    return name;
}

const char *
get_erd_xmlns(const ERD *erd)
{
    if (erd->namedQName.ns)
    {
        static char xmlns[LIMIT_NAME_LENGTH];
        char *      next = xmlns;
        char *      last = xmlns + sizeof(xmlns) - 1;

        next = memccpy(next, "xmlns", 0, last - next);
        if (next)
        {
            --next;
        }

        if (next && erd->namedQName.prefix)
        {
            next = memccpy(next, ":", 0, last - next);
            if (next)
            {
                --next;
            }
        }
        if (next && erd->namedQName.prefix)
        {
            next = memccpy(next, erd->namedQName.prefix, 0, last - next);
            if (next)
            {
                --next;
            }
        }
        if (!next)
        {
            *last = 0;
        }

        return xmlns;
    }
    else
    {
        return NULL;
    }
}

const char *
get_erd_ns(const ERD *erd)
{
    return erd->namedQName.ns;
}

// walkInfosetNode - recursively walk an infoset node and call
// VisitEventHandler methods

static const Error *
walkInfosetNode(const VisitEventHandler *handler, const InfosetBase *infoNode)
{
    const size_t      count = infoNode->erd->numChildren;
    const ERD **const childrenERDs = infoNode->erd->childrenERDs;
    const size_t *    offsets = infoNode->erd->offsets;

    // Start visiting the node
    const Error *error = handler->visitStartComplex(handler, infoNode);

    // Walk the node's children recursively
    for (size_t i = 0; i < count && !error; i++)
    {
        const size_t offset = offsets[i];
        const ERD *  childERD = childrenERDs[i];
        // We use only one of these variables below depending on typeCode
        const InfosetBase *childNode = (const InfosetBase *)((const char *)infoNode + offset);
        const void *       number = (const void *)((const char *)infoNode + offset);

        // Will need to handle more element types
        const enum TypeCode typeCode = childERD->typeCode;
        switch (typeCode)
        {
        case CHOICE:
            // Point next ERD to choice of alternative elements' ERDs
            error = infoNode->erd->initChoice(infoNode, rootElement());
            break;
        case COMPLEX:
            error = walkInfosetNode(handler, childNode);
            break;
        case PRIMITIVE_BOOLEAN:
        case PRIMITIVE_DOUBLE:
        case PRIMITIVE_FLOAT:
        case PRIMITIVE_HEXBINARY:
        case PRIMITIVE_INT16:
        case PRIMITIVE_INT32:
        case PRIMITIVE_INT64:
        case PRIMITIVE_INT8:
        case PRIMITIVE_UINT16:
        case PRIMITIVE_UINT32:
        case PRIMITIVE_UINT64:
        case PRIMITIVE_UINT8:
            error = handler->visitSimpleElem(handler, childERD, number);
            break;
        }
    }

    // End visiting the node
    if (!error)
    {
        error = handler->visitEndComplex(handler, infoNode);
    }

    return error;
}

// walkInfoset - walk an infoset and call VisitEventHandler methods

const Error *
walkInfoset(const VisitEventHandler *handler, const InfosetBase *infoset)
{
    const Error *error = handler->visitStartDocument(handler);

    if (!error)
    {
        error = walkInfosetNode(handler, infoset);
    }
    if (!error)
    {
        error = handler->visitEndDocument(handler);
    }

    return error;
}

// flushUState - flush any buffered bits not written yet

#define BYTE_WIDTH 8

void
flushUState(UState *ustate)
{
    // Do we have any bits within the fractional byte?
    if (ustate->unwritLen)
    {
        // Fill the fractional byte
        size_t num_bits_fill = BYTE_WIDTH - ustate->unwritLen;
        ustate->unwritBits <<= num_bits_fill;
        ustate->bitPos0b += num_bits_fill;
        ustate->unwritLen = 0; // Even though we don't flush until next line

        // Flush the fractional byte
        size_t count = fwrite(&ustate->unwritBits, 1, 1, ustate->stream);
        if (count < 1)
        {
            ustate->error = eof_or_error(ustate->stream);
        }
    }
}
