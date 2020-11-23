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

#include "ex_int32.h" // for generated code structs
#include <endian.h>   // for be32toh, htobe32
#include <stddef.h>   // for ptrdiff_t
#include <stdio.h>    // for NULL, fread, fwrite, size_t, FILE

// Prototypes needed for compilation

static void        c2_initSelf(c2 *instance);
static const char *c2_parseSelf(c2 *instance, const PState *pstate);
static const char *c2_unparseSelf(const c2 *instance, const UState *ustate);
static void        c1_initSelf(c1 *instance);
static const char *c1_parseSelf(c1 *instance, const PState *pstate);
static const char *c1_unparseSelf(const c1 *instance, const UState *ustate);

// Metadata singletons

static const ERD e1_ERD = {
    {
        "ex", // namedQName.prefix
        "e1", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD e2_ERD = {
    {
        "ex", // namedQName.prefix
        "e2", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD e3_ERD = {
    {
        "ex", // namedQName.prefix
        "e3", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const c2 c2_compute_ERD_offsets;

static const ptrdiff_t c2_offsets[2] = {
    (char *)&c2_compute_ERD_offsets.e2 - (char *)&c2_compute_ERD_offsets,
    (char *)&c2_compute_ERD_offsets.e3 - (char *)&c2_compute_ERD_offsets};

static const ERD *c2_childrenERDs[2] = {&e2_ERD, &e3_ERD};

static const ERD c2_ERD = {
    {
        "ex", // namedQName.prefix
        "c2", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX,                         // typeCode
    2,                               // numChildren
    c2_offsets,                      // offsets
    c2_childrenERDs,                 // childrenERDs
    (ERDInitSelf)&c2_initSelf,       // initSelf
    (ERDParseSelf)&c2_parseSelf,     // parseSelf
    (ERDUnparseSelf)&c2_unparseSelf, // unparseSelf
};

static const c1 c1_compute_ERD_offsets;

static const ptrdiff_t c1_offsets[2] = {
    (char *)&c1_compute_ERD_offsets.e1 - (char *)&c1_compute_ERD_offsets,
    (char *)&c1_compute_ERD_offsets.c2 - (char *)&c1_compute_ERD_offsets};

static const ERD *c1_childrenERDs[2] = {&e1_ERD, &c2_ERD};

static const ERD c1_ERD = {
    {
        "ex",                 // namedQName.prefix
        "c1",                 // namedQName.local
        "http://example.com", // namedQName.ns
    },
    COMPLEX,                         // typeCode
    2,                               // numChildren
    c1_offsets,                      // offsets
    c1_childrenERDs,                 // childrenERDs
    (ERDInitSelf)&c1_initSelf,       // initSelf
    (ERDParseSelf)&c1_parseSelf,     // parseSelf
    (ERDUnparseSelf)&c1_unparseSelf, // unparseSelf
};

// Return a root element to be used for parsing or unparsing

InfosetBase *
rootElement()
{
    static c1    instance;
    InfosetBase *root = &instance._base;
    c1_ERD.initSelf(root);
    return root;
}

// Methods to initialize, parse, and unparse infoset nodes

static void
c2_initSelf(c2 *instance)
{
    instance->e2 = 0xCDCDCDCD;
    instance->e3 = 0xCDCDCDCD;
    instance->_base.erd = &c2_ERD;
}

static const char *
c2_parseSelf(c2 *instance, const PState *pstate)
{
    const char *error_msg = NULL;
    if (!error_msg)
    {
        char   buffer[4];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->e2 = be32toh(*((uint32_t *)(&buffer)));
    }
    if (!error_msg)
    {
        char   buffer[4];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->e3 = be32toh(*((uint32_t *)(&buffer)));
    }
    return error_msg;
}

static const char *
c2_unparseSelf(const c2 *instance, const UState *ustate)
{
    const char *error_msg = NULL;
    if (!error_msg)
    {
        union
        {
            char     c_val[4];
            uint32_t i_val;
        } buffer;
        buffer.i_val = htobe32(instance->e2);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        union
        {
            char     c_val[4];
            uint32_t i_val;
        } buffer;
        buffer.i_val = htobe32(instance->e3);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    return error_msg;
}

static void
c1_initSelf(c1 *instance)
{
    instance->e1 = 0xCDCDCDCD;
    c2_initSelf(&instance->c2);
    instance->_base.erd = &c1_ERD;
}

static const char *
c1_parseSelf(c1 *instance, const PState *pstate)
{
    const char *error_msg = NULL;
    if (!error_msg)
    {
        char   buffer[4];
        size_t count = fread(&buffer, 1, sizeof(buffer), pstate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(pstate->stream);
        }
        instance->e1 = be32toh(*((uint32_t *)(&buffer)));
    }
    if (!error_msg)
    {
        error_msg = c2_parseSelf(&instance->c2, pstate);
    }
    return error_msg;
}

static const char *
c1_unparseSelf(const c1 *instance, const UState *ustate)
{
    const char *error_msg = NULL;
    if (!error_msg)
    {
        union
        {
            char     c_val[4];
            uint32_t i_val;
        } buffer;
        buffer.i_val = htobe32(instance->e1);
        size_t count = fwrite(buffer.c_val, 1, sizeof(buffer), ustate->stream);
        if (count < sizeof(buffer))
        {
            error_msg = eof_or_error_msg(ustate->stream);
        }
    }
    if (!error_msg)
    {
        error_msg = c2_unparseSelf(&instance->c2, ustate);
    }
    return error_msg;
}
