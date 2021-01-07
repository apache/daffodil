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

#include "ex_nums.h"
#include "parsers.h"    // for parse_be_double, parse_be_float, parse_be_int16, parse_be_int32, parse_be_int64, parse_be_int8, parse_be_uint16, parse_be_uint32, parse_be_uint64, parse_be_uint8, parse_le_double, parse_le_float, parse_le_int16, parse_le_int32, parse_le_int64, parse_le_int8, parse_le_uint16, parse_le_uint32, parse_le_uint64, parse_le_uint8
#include "unparsers.h"  // for unparse_be_double, unparse_be_float, unparse_be_int16, unparse_be_int32, unparse_be_int64, unparse_be_int8, unparse_be_uint16, unparse_be_uint32, unparse_be_uint64, unparse_be_uint8, unparse_le_double, unparse_le_float, unparse_le_int16, unparse_le_int32, unparse_le_int64, unparse_le_int8, unparse_le_uint16, unparse_le_uint32, unparse_le_uint64, unparse_le_uint8
#include <math.h>       // for NAN
#include <stddef.h>     // for NULL, ptrdiff_t

// Prototypes needed for compilation

static void array_initSelf(array *instance);
static void array_parseSelf(array *instance, PState *pstate);
static void array_unparseSelf(const array *instance, UState *ustate);
static void bigEndian_initSelf(bigEndian *instance);
static void bigEndian_parseSelf(bigEndian *instance, PState *pstate);
static void bigEndian_unparseSelf(const bigEndian *instance, UState *ustate);
static void littleEndian_initSelf(littleEndian *instance);
static void littleEndian_parseSelf(littleEndian *instance, PState *pstate);
static void littleEndian_unparseSelf(const littleEndian *instance, UState *ustate);
static void ex_nums_initSelf(ex_nums *instance);
static void ex_nums_parseSelf(ex_nums *instance, PState *pstate);
static void ex_nums_unparseSelf(const ex_nums *instance, UState *ustate);

// Metadata singletons

static const ERD be_int16_array_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "be_int16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_float_array_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "be_float", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const array array_compute_offsets;

static const ptrdiff_t array_offsets[6] = {
    (const char *)&array_compute_offsets.be_int16[0] - (const char *)&array_compute_offsets,
    (const char *)&array_compute_offsets.be_int16[1] - (const char *)&array_compute_offsets,
    (const char *)&array_compute_offsets.be_int16[2] - (const char *)&array_compute_offsets,
    (const char *)&array_compute_offsets.be_float[0] - (const char *)&array_compute_offsets,
    (const char *)&array_compute_offsets.be_float[1] - (const char *)&array_compute_offsets,
    (const char *)&array_compute_offsets.be_float[2] - (const char *)&array_compute_offsets
};

static const ERD *array_childrenERDs[6] = {
    &be_int16_array_ex_nums__ERD,
    &be_int16_array_ex_nums__ERD,
    &be_int16_array_ex_nums__ERD,
    &be_float_array_ex_nums__ERD,
    &be_float_array_ex_nums__ERD,
    &be_float_array_ex_nums__ERD
};

static const ERD array_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "array", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX,                         // typeCode
    6,                               // numChildren
    array_offsets,                      // offsets
    array_childrenERDs,                 // childrenERDs
    (ERDInitSelf)&array_initSelf,       // initSelf
    (ERDParseSelf)&array_parseSelf,     // parseSelf
    (ERDUnparseSelf)&array_unparseSelf, // unparseSelf
};

static const ERD be_double_bigEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "be_double", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_float_bigEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "be_float", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_uint64_bigEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT64, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_uint32_bigEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_uint16_bigEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_uint8_bigEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT8, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_int64_bigEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "be_int64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_int32_bigEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "be_int32", // namedQName.local
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

static const ERD be_int16_bigEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "be_int16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD be_int8_bigEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "be_int8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT8, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const bigEndian bigEndian_compute_offsets;

static const ptrdiff_t bigEndian_offsets[10] = {
    (const char *)&bigEndian_compute_offsets.be_double - (const char *)&bigEndian_compute_offsets,
    (const char *)&bigEndian_compute_offsets.be_float - (const char *)&bigEndian_compute_offsets,
    (const char *)&bigEndian_compute_offsets.be_uint64 - (const char *)&bigEndian_compute_offsets,
    (const char *)&bigEndian_compute_offsets.be_uint32 - (const char *)&bigEndian_compute_offsets,
    (const char *)&bigEndian_compute_offsets.be_uint16 - (const char *)&bigEndian_compute_offsets,
    (const char *)&bigEndian_compute_offsets.be_uint8 - (const char *)&bigEndian_compute_offsets,
    (const char *)&bigEndian_compute_offsets.be_int64 - (const char *)&bigEndian_compute_offsets,
    (const char *)&bigEndian_compute_offsets.be_int32 - (const char *)&bigEndian_compute_offsets,
    (const char *)&bigEndian_compute_offsets.be_int16 - (const char *)&bigEndian_compute_offsets,
    (const char *)&bigEndian_compute_offsets.be_int8 - (const char *)&bigEndian_compute_offsets
};

static const ERD *bigEndian_childrenERDs[10] = {
    &be_double_bigEndian_ex_nums__ERD,
    &be_float_bigEndian_ex_nums__ERD,
    &be_uint64_bigEndian_ex_nums__ERD,
    &be_uint32_bigEndian_ex_nums__ERD,
    &be_uint16_bigEndian_ex_nums__ERD,
    &be_uint8_bigEndian_ex_nums__ERD,
    &be_int64_bigEndian_ex_nums__ERD,
    &be_int32_bigEndian_ex_nums__ERD,
    &be_int16_bigEndian_ex_nums__ERD,
    &be_int8_bigEndian_ex_nums__ERD
};

static const ERD bigEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "bigEndian", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX,                         // typeCode
    10,                               // numChildren
    bigEndian_offsets,                      // offsets
    bigEndian_childrenERDs,                 // childrenERDs
    (ERDInitSelf)&bigEndian_initSelf,       // initSelf
    (ERDParseSelf)&bigEndian_parseSelf,     // parseSelf
    (ERDUnparseSelf)&bigEndian_unparseSelf, // unparseSelf
};

static const ERD le_uint64_littleEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT64, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_uint32_littleEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_uint16_littleEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_uint8_littleEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT8, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_int64_littleEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "le_int64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_int32_littleEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "le_int32", // namedQName.local
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

static const ERD le_int16_littleEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "le_int16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_int8_littleEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "le_int8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT8, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_float_littleEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "le_float", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const ERD le_double_littleEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "le_double", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0,               // numChildren
    NULL,            // offsets
    NULL,            // childrenERDs
    NULL,            // initSelf
    NULL,            // parseSelf
    NULL,            // unparseSelf
};

static const littleEndian littleEndian_compute_offsets;

static const ptrdiff_t littleEndian_offsets[10] = {
    (const char *)&littleEndian_compute_offsets.le_uint64 - (const char *)&littleEndian_compute_offsets,
    (const char *)&littleEndian_compute_offsets.le_uint32 - (const char *)&littleEndian_compute_offsets,
    (const char *)&littleEndian_compute_offsets.le_uint16 - (const char *)&littleEndian_compute_offsets,
    (const char *)&littleEndian_compute_offsets.le_uint8 - (const char *)&littleEndian_compute_offsets,
    (const char *)&littleEndian_compute_offsets.le_int64 - (const char *)&littleEndian_compute_offsets,
    (const char *)&littleEndian_compute_offsets.le_int32 - (const char *)&littleEndian_compute_offsets,
    (const char *)&littleEndian_compute_offsets.le_int16 - (const char *)&littleEndian_compute_offsets,
    (const char *)&littleEndian_compute_offsets.le_int8 - (const char *)&littleEndian_compute_offsets,
    (const char *)&littleEndian_compute_offsets.le_float - (const char *)&littleEndian_compute_offsets,
    (const char *)&littleEndian_compute_offsets.le_double - (const char *)&littleEndian_compute_offsets
};

static const ERD *littleEndian_childrenERDs[10] = {
    &le_uint64_littleEndian_ex_nums__ERD,
    &le_uint32_littleEndian_ex_nums__ERD,
    &le_uint16_littleEndian_ex_nums__ERD,
    &le_uint8_littleEndian_ex_nums__ERD,
    &le_int64_littleEndian_ex_nums__ERD,
    &le_int32_littleEndian_ex_nums__ERD,
    &le_int16_littleEndian_ex_nums__ERD,
    &le_int8_littleEndian_ex_nums__ERD,
    &le_float_littleEndian_ex_nums__ERD,
    &le_double_littleEndian_ex_nums__ERD
};

static const ERD littleEndian_ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "littleEndian", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX,                         // typeCode
    10,                               // numChildren
    littleEndian_offsets,                      // offsets
    littleEndian_childrenERDs,                 // childrenERDs
    (ERDInitSelf)&littleEndian_initSelf,       // initSelf
    (ERDParseSelf)&littleEndian_parseSelf,     // parseSelf
    (ERDUnparseSelf)&littleEndian_unparseSelf, // unparseSelf
};

static const ex_nums ex_nums_compute_offsets;

static const ptrdiff_t ex_nums_offsets[3] = {
    (const char *)&ex_nums_compute_offsets.array - (const char *)&ex_nums_compute_offsets,
    (const char *)&ex_nums_compute_offsets.bigEndian - (const char *)&ex_nums_compute_offsets,
    (const char *)&ex_nums_compute_offsets.littleEndian - (const char *)&ex_nums_compute_offsets
};

static const ERD *ex_nums_childrenERDs[3] = {
    &array_ex_nums__ERD,
    &bigEndian_ex_nums__ERD,
    &littleEndian_ex_nums__ERD
};

static const ERD ex_nums__ERD = {
    {
        NULL, // namedQName.prefix
        "ex_nums", // namedQName.local
        "http://example.com", // namedQName.ns
    },
    COMPLEX,                         // typeCode
    3,                               // numChildren
    ex_nums_offsets,                      // offsets
    ex_nums_childrenERDs,                 // childrenERDs
    (ERDInitSelf)&ex_nums_initSelf,       // initSelf
    (ERDParseSelf)&ex_nums_parseSelf,     // parseSelf
    (ERDUnparseSelf)&ex_nums_unparseSelf, // unparseSelf
};

// Return a root element to be used for parsing or unparsing

extern InfosetBase *
rootElement(void)
{
    static ex_nums instance;
    InfosetBase *root = &instance._base;
    ex_nums__ERD.initSelf(root);
    return root;
}

// Methods to initialize, parse, and unparse infoset nodes

static void
array_initSelf(array *instance)
{
    instance->be_int16[0] = 0xCCCC;
    instance->be_int16[1] = 0xCCCC;
    instance->be_int16[2] = 0xCCCC;
    instance->be_float[0] = NAN;
    instance->be_float[1] = NAN;
    instance->be_float[2] = NAN;
    instance->_base.erd = &array_ex_nums__ERD;
}

static void
array_parseSelf(array *instance, PState *pstate)
{
    parse_be_int16(&instance->be_int16[0], pstate);
    parse_be_int16(&instance->be_int16[1], pstate);
    parse_be_int16(&instance->be_int16[2], pstate);
    parse_be_float(&instance->be_float[0], pstate);
    parse_be_float(&instance->be_float[1], pstate);
    parse_be_float(&instance->be_float[2], pstate);
}

static void
array_unparseSelf(const array *instance, UState *ustate)
{
    unparse_be_int16(instance->be_int16[0], ustate);
    unparse_be_int16(instance->be_int16[1], ustate);
    unparse_be_int16(instance->be_int16[2], ustate);
    unparse_be_float(instance->be_float[0], ustate);
    unparse_be_float(instance->be_float[1], ustate);
    unparse_be_float(instance->be_float[2], ustate);
}

static void
bigEndian_initSelf(bigEndian *instance)
{
    instance->be_double = NAN;
    instance->be_float = NAN;
    instance->be_uint64 = 0xCCCCCCCCCCCCCCCC;
    instance->be_uint32 = 0xCCCCCCCC;
    instance->be_uint16 = 0xCCCC;
    instance->be_uint8 = 0xCC;
    instance->be_int64 = 0xCCCCCCCCCCCCCCCC;
    instance->be_int32 = 0xCCCCCCCC;
    instance->be_int16 = 0xCCCC;
    instance->be_int8 = 0xCC;
    instance->_base.erd = &bigEndian_ex_nums__ERD;
}

static void
bigEndian_parseSelf(bigEndian *instance, PState *pstate)
{
    parse_be_double(&instance->be_double, pstate);
    parse_be_float(&instance->be_float, pstate);
    parse_be_uint64(&instance->be_uint64, pstate);
    parse_be_uint32(&instance->be_uint32, pstate);
    parse_be_uint16(&instance->be_uint16, pstate);
    parse_be_uint8(&instance->be_uint8, pstate);
    parse_be_int64(&instance->be_int64, pstate);
    parse_be_int32(&instance->be_int32, pstate);
    parse_be_int16(&instance->be_int16, pstate);
    parse_be_int8(&instance->be_int8, pstate);
}

static void
bigEndian_unparseSelf(const bigEndian *instance, UState *ustate)
{
    unparse_be_double(instance->be_double, ustate);
    unparse_be_float(instance->be_float, ustate);
    unparse_be_uint64(instance->be_uint64, ustate);
    unparse_be_uint32(instance->be_uint32, ustate);
    unparse_be_uint16(instance->be_uint16, ustate);
    unparse_be_uint8(instance->be_uint8, ustate);
    unparse_be_int64(instance->be_int64, ustate);
    unparse_be_int32(instance->be_int32, ustate);
    unparse_be_int16(instance->be_int16, ustate);
    unparse_be_int8(instance->be_int8, ustate);
}

static void
littleEndian_initSelf(littleEndian *instance)
{
    instance->le_uint64 = 0xCCCCCCCCCCCCCCCC;
    instance->le_uint32 = 0xCCCCCCCC;
    instance->le_uint16 = 0xCCCC;
    instance->le_uint8 = 0xCC;
    instance->le_int64 = 0xCCCCCCCCCCCCCCCC;
    instance->le_int32 = 0xCCCCCCCC;
    instance->le_int16 = 0xCCCC;
    instance->le_int8 = 0xCC;
    instance->le_float = NAN;
    instance->le_double = NAN;
    instance->_base.erd = &littleEndian_ex_nums__ERD;
}

static void
littleEndian_parseSelf(littleEndian *instance, PState *pstate)
{
    parse_le_uint64(&instance->le_uint64, pstate);
    parse_le_uint32(&instance->le_uint32, pstate);
    parse_le_uint16(&instance->le_uint16, pstate);
    parse_le_uint8(&instance->le_uint8, pstate);
    parse_le_int64(&instance->le_int64, pstate);
    parse_le_int32(&instance->le_int32, pstate);
    parse_le_int16(&instance->le_int16, pstate);
    parse_le_int8(&instance->le_int8, pstate);
    parse_le_float(&instance->le_float, pstate);
    parse_le_double(&instance->le_double, pstate);
}

static void
littleEndian_unparseSelf(const littleEndian *instance, UState *ustate)
{
    unparse_le_uint64(instance->le_uint64, ustate);
    unparse_le_uint32(instance->le_uint32, ustate);
    unparse_le_uint16(instance->le_uint16, ustate);
    unparse_le_uint8(instance->le_uint8, ustate);
    unparse_le_int64(instance->le_int64, ustate);
    unparse_le_int32(instance->le_int32, ustate);
    unparse_le_int16(instance->le_int16, ustate);
    unparse_le_int8(instance->le_int8, ustate);
    unparse_le_float(instance->le_float, ustate);
    unparse_le_double(instance->le_double, ustate);
}

static void
ex_nums_initSelf(ex_nums *instance)
{
    array_initSelf(&instance->array);
    bigEndian_initSelf(&instance->bigEndian);
    littleEndian_initSelf(&instance->littleEndian);
    instance->_base.erd = &ex_nums__ERD;
}

static void
ex_nums_parseSelf(ex_nums *instance, PState *pstate)
{
    array_parseSelf(&instance->array, pstate);
    bigEndian_parseSelf(&instance->bigEndian, pstate);
    littleEndian_parseSelf(&instance->littleEndian, pstate);
}

static void
ex_nums_unparseSelf(const ex_nums *instance, UState *ustate)
{
    array_unparseSelf(&instance->array, ustate);
    bigEndian_unparseSelf(&instance->bigEndian, ustate);
    littleEndian_unparseSelf(&instance->littleEndian, ustate);
}

