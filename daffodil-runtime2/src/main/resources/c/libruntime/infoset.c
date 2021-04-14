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

#include "infoset.h"
#include <string.h>  // for memccpy
#include "errors.h"  // for Error, LIMIT_NAME_LENGTH

// get_erd_name, get_erd_xmlns, get_erd_ns - get name and xmlns
// attribute/value from ERD to use on XML element

const char *
get_erd_name(const ERD *erd)
{
    static char name[LIMIT_NAME_LENGTH];
    char *      next = name;
    char *      last = name + sizeof(name) - 1;

    if (next && erd->namedQName.prefix)
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
    const Error *error = NULL;

    // Start visiting the node
    if (!error)
    {
        error = handler->visitStartComplex(handler, infoNode);
    }

    // Walk the node's children recursively
    const size_t      count = infoNode->erd->numChildren;
    const ERD **const childrenERDs = infoNode->erd->childrenERDs;
    const size_t *    offsets = infoNode->erd->offsets;

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
        case PRIMITIVE_INT16:
        case PRIMITIVE_INT32:
        case PRIMITIVE_INT64:
        case PRIMITIVE_INT8:
        case PRIMITIVE_UINT16:
        case PRIMITIVE_UINT32:
        case PRIMITIVE_UINT64:
        case PRIMITIVE_UINT8:
            error = handler->visitNumberElem(handler, childERD, number);
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
    const Error *error = NULL;

    if (!error)
    {
        error = handler->visitStartDocument(handler);
    }
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
