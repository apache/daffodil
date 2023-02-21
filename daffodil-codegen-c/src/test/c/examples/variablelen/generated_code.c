// clang-format off
#include "generated_code.h"
#include <stdbool.h>    // for false, bool, true
#include <stddef.h>     // for NULL, size_t
#include <string.h>     // for memcmp
#include "errors.h"     // for Error, PState, UState, ERR_CHOICE_KEY, Error::(anonymous), UNUSED
#include "parsers.h"    // for alloc_hexBinary, parse_hexBinary, parse_be_float, parse_be_int16, parse_validate_fixed, parse_be_bool32, parse_be_bool16, parse_be_int32, parse_be_uint16, parse_be_uint32, parse_le_bool32, parse_le_int64, parse_le_uint16, parse_le_uint8, parse_be_bool8, parse_be_double, parse_be_int64, parse_be_int8, parse_be_uint64, parse_be_uint8, parse_le_bool16, parse_le_bool8, parse_le_double, parse_le_float, parse_le_int16, parse_le_int32, parse_le_int8, parse_le_uint32, parse_le_uint64
#include "unparsers.h"  // for unparse_hexBinary, unparse_be_float, unparse_be_int16, unparse_validate_fixed, unparse_be_bool32, unparse_be_bool16, unparse_be_int32, unparse_be_uint16, unparse_be_uint32, unparse_le_bool32, unparse_le_int64, unparse_le_uint16, unparse_le_uint8, unparse_be_bool8, unparse_be_double, unparse_be_int64, unparse_be_int8, unparse_be_uint64, unparse_be_uint8, unparse_le_bool16, unparse_le_bool8, unparse_le_double, unparse_le_float, unparse_le_int16, unparse_le_int32, unparse_le_int8, unparse_le_uint32, unparse_le_uint64
// clang-format on

// Declare prototypes for easier compilation

static void array_variablelen_expressionType_expressionElement__parseSelf(expressionElement_ *instance, PState *pstate);
static void array_variablelen_expressionType_expressionElement__unparseSelf(const expressionElement_ *instance, UState *ustate);
static size_t array_variablelen_expressionType_expressionElement__getArraySize(const expressionElement_ *instance);
static void array_after_expressionType_expressionElement__parseSelf(expressionElement_ *instance, PState *pstate);
static void array_after_expressionType_expressionElement__unparseSelf(const expressionElement_ *instance, UState *ustate);
static size_t array_after_expressionType_expressionElement__getArraySize(const expressionElement_ *instance);
static void expressionElement__parseSelf(expressionElement_ *instance, PState *pstate);
static void expressionElement__unparseSelf(const expressionElement_ *instance, UState *ustate);

// Define metadata for the infoset

static const ERD before_expressionType_ERD = {
    {
        NULL, // namedQName.prefix
        "before", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD variablelen_size_expressionType_ERD = {
    {
        NULL, // namedQName.prefix
        "variablelen_size", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD variablelen_expressionType_ERD = {
    {
        NULL, // namedQName.prefix
        "variablelen", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const expressionElement_ array_variablelen_expressionType_expressionElement__compute_offsets;

static const size_t array_variablelen_expressionType_expressionElement__offsets[1] = {
    (const char *)&array_variablelen_expressionType_expressionElement__compute_offsets.variablelen[1] - (const char *)&array_variablelen_expressionType_expressionElement__compute_offsets.variablelen[0]
};

static const ERD *const array_variablelen_expressionType_expressionElement__childrenERDs[1] = {
    &variablelen_expressionType_ERD
};

static const ERD array_variablelen_expressionType_expressionElement_ERD = {
    {
        NULL, // namedQName.prefix
        "variablelen", // namedQName.local
        NULL, // namedQName.ns
    },
    ARRAY, // typeCode
    16, // maxOccurs
    array_variablelen_expressionType_expressionElement__offsets, // offsets
    array_variablelen_expressionType_expressionElement__childrenERDs, // childrenERDs
    (ERDParseSelf)&array_variablelen_expressionType_expressionElement__parseSelf, // parseSelf
    (ERDUnparseSelf)&array_variablelen_expressionType_expressionElement__unparseSelf, // unparseSelf
    {.getArraySize = (GetArraySize)&array_variablelen_expressionType_expressionElement__getArraySize} // getArraySize
};

static const ERD after_expressionType_ERD = {
    {
        NULL, // namedQName.prefix
        "after", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const expressionElement_ array_after_expressionType_expressionElement__compute_offsets;

static const size_t array_after_expressionType_expressionElement__offsets[1] = {
    (const char *)&array_after_expressionType_expressionElement__compute_offsets.after[1] - (const char *)&array_after_expressionType_expressionElement__compute_offsets.after[0]
};

static const ERD *const array_after_expressionType_expressionElement__childrenERDs[1] = {
    &after_expressionType_ERD
};

static const ERD array_after_expressionType_expressionElement_ERD = {
    {
        NULL, // namedQName.prefix
        "after", // namedQName.local
        NULL, // namedQName.ns
    },
    ARRAY, // typeCode
    2, // maxOccurs
    array_after_expressionType_expressionElement__offsets, // offsets
    array_after_expressionType_expressionElement__childrenERDs, // childrenERDs
    (ERDParseSelf)&array_after_expressionType_expressionElement__parseSelf, // parseSelf
    (ERDUnparseSelf)&array_after_expressionType_expressionElement__unparseSelf, // unparseSelf
    {.getArraySize = (GetArraySize)&array_after_expressionType_expressionElement__getArraySize} // getArraySize
};

static const expressionElement_ expressionElement__compute_offsets;

static const size_t expressionElement__offsets[4] = {
    (const char *)&expressionElement__compute_offsets.before - (const char *)&expressionElement__compute_offsets,
    (const char *)&expressionElement__compute_offsets.variablelen_size - (const char *)&expressionElement__compute_offsets,
    (const char *)&expressionElement__compute_offsets.variablelen[0] - (const char *)&expressionElement__compute_offsets,
    (const char *)&expressionElement__compute_offsets.after[0] - (const char *)&expressionElement__compute_offsets
};

static const ERD *const expressionElement__childrenERDs[4] = {
    &before_expressionType_ERD,
    &variablelen_size_expressionType_ERD,
    &array_variablelen_expressionType_expressionElement_ERD,
    &array_after_expressionType_expressionElement_ERD
};

static const ERD expressionElement_ERD = {
    {
        NULL, // namedQName.prefix
        "expressionElement", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX, // typeCode
    4, // numChildren
    expressionElement__offsets, // offsets
    expressionElement__childrenERDs, // childrenERDs
    (ERDParseSelf)&expressionElement__parseSelf, // parseSelf
    (ERDUnparseSelf)&expressionElement__unparseSelf, // unparseSelf
    {NULL} // initChoice
};

// Initialize, parse, and unparse nodes of the infoset

static void
array_variablelen_expressionType_expressionElement__initERD(expressionElement_ *instance, InfosetBase *parent)
{
    UNUSED(instance);
    UNUSED(parent);
}

static void
array_variablelen_expressionType_expressionElement__parseSelf(expressionElement_ *instance, PState *pstate)
{
    const size_t arraySize = array_variablelen_expressionType_expressionElement__getArraySize(instance);
    parse_check_bounds("array_variablelen_expressionType_expressionElement_", arraySize, 0, 16, pstate);
    if (pstate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        parse_be_uint32(&instance->variablelen[i], 32, pstate);
        if (pstate->error) return;
    }
}

static void
array_variablelen_expressionType_expressionElement__unparseSelf(const expressionElement_ *instance, UState *ustate)
{
    const size_t arraySize = array_variablelen_expressionType_expressionElement__getArraySize(instance);
    unparse_check_bounds("array_variablelen_expressionType_expressionElement_", arraySize, 0, 16, ustate);
    if (ustate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        unparse_be_uint32(instance->variablelen[i], 32, ustate);
        if (ustate->error) return;
    }
}

static size_t
array_variablelen_expressionType_expressionElement__getArraySize(const expressionElement_ *instance)
{
    return instance->variablelen_size;
}

static void
array_after_expressionType_expressionElement__initERD(expressionElement_ *instance, InfosetBase *parent)
{
    UNUSED(instance);
    UNUSED(parent);
}

static void
array_after_expressionType_expressionElement__parseSelf(expressionElement_ *instance, PState *pstate)
{
    const size_t arraySize = array_after_expressionType_expressionElement__getArraySize(instance);
    parse_check_bounds("array_after_expressionType_expressionElement_", arraySize, 2, 2, pstate);
    if (pstate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        parse_be_uint32(&instance->after[i], 32, pstate);
        if (pstate->error) return;
    }
}

static void
array_after_expressionType_expressionElement__unparseSelf(const expressionElement_ *instance, UState *ustate)
{
    const size_t arraySize = array_after_expressionType_expressionElement__getArraySize(instance);
    unparse_check_bounds("array_after_expressionType_expressionElement_", arraySize, 2, 2, ustate);
    if (ustate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        unparse_be_uint32(instance->after[i], 32, ustate);
        if (ustate->error) return;
    }
}

static size_t
array_after_expressionType_expressionElement__getArraySize(const expressionElement_ *instance)
{
    UNUSED(instance);
    return 2;
}

static void
expressionElement__initERD(expressionElement_ *instance, InfosetBase *parent)
{
    instance->_base.erd = &expressionElement_ERD;
    instance->_base.parent = parent;
    array_variablelen_expressionType_expressionElement__initERD(instance, parent);
    array_after_expressionType_expressionElement__initERD(instance, parent);
}

static void
expressionElement__parseSelf(expressionElement_ *instance, PState *pstate)
{
    parse_be_uint32(&instance->before, 32, pstate);
    if (pstate->error) return;
    parse_be_uint32(&instance->variablelen_size, 32, pstate);
    if (pstate->error) return;
    array_variablelen_expressionType_expressionElement__parseSelf(instance, pstate);
    if (pstate->error) return;
    array_after_expressionType_expressionElement__parseSelf(instance, pstate);
    if (pstate->error) return;
}

static void
expressionElement__unparseSelf(const expressionElement_ *instance, UState *ustate)
{
    unparse_be_uint32(instance->before, 32, ustate);
    if (ustate->error) return;
    unparse_be_uint32(instance->variablelen_size, 32, ustate);
    if (ustate->error) return;
    array_variablelen_expressionType_expressionElement__unparseSelf(instance, ustate);
    if (ustate->error) return;
    array_after_expressionType_expressionElement__unparseSelf(instance, ustate);
    if (ustate->error) return;
}

// Return a root element for parsing or unparsing the infoset

InfosetBase *
rootElement(void)
{
    static bool initialized;
    static expressionElement_ root;
    if (!initialized)
    {
        expressionElement__initERD(&root, (InfosetBase *)&root);
        initialized = true;
    }
    return &root._base;
}
