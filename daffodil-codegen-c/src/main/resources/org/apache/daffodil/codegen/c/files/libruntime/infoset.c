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

// auto-maintained by iwyu
// clang-format off
#include "infoset.h"
#include <string.h>     // for memccpy
#include "errors.h"     // for Error, LIMIT_NAME_LENGTH
#include "parsers.h"    // for no_leftover_data
#include "unparsers.h"  // for flush_fragment_byte
// clang-format on

// Declare prototypes for easier compilation

static const Error *walk_infoset_node(const VisitEventHandler *handler, const InfosetBase *infoNode,
                                      const ERD *childERD, const void *child);
static const Error *walk_array(const VisitEventHandler *handler, const InfosetBase *infoNode,
                               const ERD *arrayERD, const void *child);
static const Error *walk_infoset_node_children(const VisitEventHandler *handler, const InfosetBase *infoNode);

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
        char *next = xmlns;
        char *last = xmlns + sizeof(xmlns) - 1;

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

// parse_data - parse an input stream into an infoset, check for
// leftover data, and return any errors in pstate

void
parse_data(InfosetBase *infoset, PState *pstate)
{
    infoset->erd->parseSelf(infoset, pstate);
    no_leftover_data(pstate);
}

// unparse_infoset - unparse an infoset to an output stream, flush the
// fragment byte if not done yet, and return any errors in ustate

void
unparse_infoset(InfosetBase *infoset, UState *ustate)
{
    infoset->erd->unparseSelf(infoset, ustate);
    const uint8_t fill_byte = '\0';
    flush_fragment_byte(fill_byte, ustate);
}

// walk_infoset - walk an infoset and call VisitEventHandler methods

const Error *
walk_infoset(const VisitEventHandler *handler, const InfosetBase *infoNode)
{
    const Error *error = handler->visitStartDocument(handler);

    if (!error)
    {
        const ERD *childERD = infoNode->erd;
        const size_t childOffset = childERD->numChildren ? 0 : childERD->childrenOffsets[0];
        const void *child = (const void *)((const char *)infoNode + childOffset);

        error = walk_infoset_node(handler, infoNode, childERD, child);
    }
    if (!error)
    {
        error = handler->visitEndDocument(handler);
    }

    return error;
}

// walk_infoset_node - walk an infoset node and call VisitEventHandler
// methods

static const Error *
walk_infoset_node(const VisitEventHandler *handler, const InfosetBase *infoNode, const ERD *childERD,
                  const void *child)
{
    // Get the type of child to walk
    const Error *error = NULL;
    const InfosetBase *childNode = (const InfosetBase *)child;
    const enum TypeCode typeCode = childERD->typeCode;

    // Walk the child appropriately depending on its type
    switch (typeCode)
    {
    case ARRAY:
        // Walk an array recursively
        error = walk_array(handler, infoNode, childERD, child);
        break;
    case CHOICE:
        // Point next ERD to choice of alternative elements' ERDs
        error = infoNode->erd->initChoice(infoNode);
        break;
    case COMPLEX:
        // Walk a node's children recursively
        error = walk_infoset_node_children(handler, childNode);
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
        // Visit a simple type child
        error = handler->visitSimpleElem(handler, childERD, child);
        break;
    }

    return error;
}

// walk_array - walk each element of an array and call
// VisitEventHandler methods

static const Error *
walk_array(const VisitEventHandler *handler, const InfosetBase *infoNode, const ERD *arrayERD,
           const void *child)
{
    // Get the array's size, type of its elements, and offset between its elements
    const Error *error = NULL;
    const size_t arraySize = arrayERD->getArraySize(infoNode);
    const ERD *childERD = arrayERD->childrenERDs[0];
    const size_t childOffset = arrayERD->childrenOffsets[0];

    // Walk each element of the array
    for (size_t i = 0; i < arraySize && !error; i++)
    {
        // Walk an element of the array recursively
        error = walk_infoset_node(handler, infoNode, childERD, child);

        // Increment child by childOffset in order to walk the next element
        child = (const char *)child + childOffset;
    }

    return error;
}

// walk_infoset_node_children - walk each child of an infoset node and
// call VisitEventHandler methods

static const Error *
walk_infoset_node_children(const VisitEventHandler *handler, const InfosetBase *infoNode)
{
    const size_t numChildren = infoNode->erd->numChildren;
    const ERD *const *const childrenERDs = infoNode->erd->childrenERDs;
    const size_t *childrenOffsets = infoNode->erd->childrenOffsets;

    // Start visiting the node
    const Error *error = handler->visitStartComplex(handler, infoNode);

    // Walk each child of the node
    for (size_t i = 0; i < numChildren && !error; i++)
    {
        const ERD *childERD = childrenERDs[i];
        const size_t childOffset = childrenOffsets[i];
        const void *child = (const void *)((const char *)infoNode + childOffset);

        error = walk_infoset_node(handler, infoNode, childERD, child);
    }

    // End visiting the node
    if (!error)
    {
        error = handler->visitEndComplex(handler, infoNode);
    }

    return error;
}
