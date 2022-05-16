// clang-format off
#include "generated_code.h"
#include <math.h>       // for NAN
#include <stdbool.h>    // for false, bool, true
#include <stddef.h>     // for NULL, size_t
#include <string.h>     // for memset, memcmp
#include "errors.h"     // for Error, PState, UState, ERR_CHOICE_KEY, Error::(anonymous), UNUSED
#include "parsers.h"    // for alloc_hexBinary, parse_hexBinary, parse_be_float, parse_be_int16, parse_validate_fixed, parse_be_bool32, parse_be_bool16, parse_be_int32, parse_be_uint16, parse_be_uint32, parse_le_bool32, parse_le_int64, parse_le_uint16, parse_le_uint8, parse_be_bool8, parse_be_double, parse_be_int64, parse_be_int8, parse_be_uint64, parse_be_uint8, parse_le_bool16, parse_le_bool8, parse_le_double, parse_le_float, parse_le_int16, parse_le_int32, parse_le_int8, parse_le_uint32, parse_le_uint64
#include "unparsers.h"  // for unparse_hexBinary, unparse_be_float, unparse_be_int16, unparse_validate_fixed, unparse_be_bool32, unparse_be_bool16, unparse_be_int32, unparse_be_uint16, unparse_be_uint32, unparse_le_bool32, unparse_le_int64, unparse_le_uint16, unparse_le_uint8, unparse_be_bool8, unparse_be_double, unparse_be_int64, unparse_be_int8, unparse_be_uint64, unparse_be_uint8, unparse_le_bool16, unparse_le_bool8, unparse_le_double, unparse_le_float, unparse_le_int16, unparse_le_int32, unparse_le_int8, unparse_le_uint32, unparse_le_uint64
// clang-format on

// Declare prototypes for easier compilation

static void foo_data_NestedUnionType__parseSelf(foo_data_NestedUnionType_ *instance, PState *pstate);
static void foo_data_NestedUnionType__unparseSelf(const foo_data_NestedUnionType_ *instance, UState *ustate);
static void bar_data_NestedUnionType__parseSelf(bar_data_NestedUnionType_ *instance, PState *pstate);
static void bar_data_NestedUnionType__unparseSelf(const bar_data_NestedUnionType_ *instance, UState *ustate);
static const Error *data_NestedUnionType__initChoice(data_NestedUnionType_ *instance, const NestedUnion_ *rootElement);
static void data_NestedUnionType__parseSelf(data_NestedUnionType_ *instance, PState *pstate);
static void data_NestedUnionType__unparseSelf(const data_NestedUnionType_ *instance, UState *ustate);
static void NestedUnion__parseSelf(NestedUnion_ *instance, PState *pstate);
static void NestedUnion__unparseSelf(const NestedUnion_ *instance, UState *ustate);

// Define metadata for the infoset

static const ERD tag_NestedUnionType_ERD = {
    {
        NULL, // namedQName.prefix
        "tag", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD _choice_data_NestedUnionType_ERD = {
    {
        NULL, // namedQName.prefix
        "_choice", // namedQName.local
        NULL, // namedQName.ns
    },
    CHOICE, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD a_FooType_ERD = {
    {
        NULL, // namedQName.prefix
        "a", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD b_FooType_ERD = {
    {
        NULL, // namedQName.prefix
        "b", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD c_FooType_ERD = {
    {
        NULL, // namedQName.prefix
        "c", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const foo_data_NestedUnionType_ foo_data_NestedUnionType__compute_offsets;

static const size_t foo_data_NestedUnionType__offsets[3] = {
    (const char *)&foo_data_NestedUnionType__compute_offsets.a - (const char *)&foo_data_NestedUnionType__compute_offsets,
    (const char *)&foo_data_NestedUnionType__compute_offsets.b - (const char *)&foo_data_NestedUnionType__compute_offsets,
    (const char *)&foo_data_NestedUnionType__compute_offsets.c - (const char *)&foo_data_NestedUnionType__compute_offsets
};

static const ERD *foo_data_NestedUnionType__childrenERDs[3] = {
    &a_FooType_ERD,
    &b_FooType_ERD,
    &c_FooType_ERD
};

static const ERD foo_data_NestedUnionType_ERD = {
    {
        NULL, // namedQName.prefix
        "foo", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX, // typeCode
    3, // numChildren
    foo_data_NestedUnionType__offsets, // offsets
    foo_data_NestedUnionType__childrenERDs, // childrenERDs
    (ERDParseSelf)&foo_data_NestedUnionType__parseSelf, // parseSelf
    (ERDUnparseSelf)&foo_data_NestedUnionType__unparseSelf, // unparseSelf
    NULL // initChoice
};

static const ERD x_BarType_ERD = {
    {
        NULL, // namedQName.prefix
        "x", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD y_BarType_ERD = {
    {
        NULL, // namedQName.prefix
        "y", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD z_BarType_ERD = {
    {
        NULL, // namedQName.prefix
        "z", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const bar_data_NestedUnionType_ bar_data_NestedUnionType__compute_offsets;

static const size_t bar_data_NestedUnionType__offsets[3] = {
    (const char *)&bar_data_NestedUnionType__compute_offsets.x - (const char *)&bar_data_NestedUnionType__compute_offsets,
    (const char *)&bar_data_NestedUnionType__compute_offsets.y - (const char *)&bar_data_NestedUnionType__compute_offsets,
    (const char *)&bar_data_NestedUnionType__compute_offsets.z - (const char *)&bar_data_NestedUnionType__compute_offsets
};

static const ERD *bar_data_NestedUnionType__childrenERDs[3] = {
    &x_BarType_ERD,
    &y_BarType_ERD,
    &z_BarType_ERD
};

static const ERD bar_data_NestedUnionType_ERD = {
    {
        NULL, // namedQName.prefix
        "bar", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX, // typeCode
    3, // numChildren
    bar_data_NestedUnionType__offsets, // offsets
    bar_data_NestedUnionType__childrenERDs, // childrenERDs
    (ERDParseSelf)&bar_data_NestedUnionType__parseSelf, // parseSelf
    (ERDUnparseSelf)&bar_data_NestedUnionType__unparseSelf, // unparseSelf
    NULL // initChoice
};

static const data_NestedUnionType_ data_NestedUnionType__compute_offsets;

static const size_t data_NestedUnionType__offsets[3] = {
    (const char *)&data_NestedUnionType__compute_offsets._choice - (const char *)&data_NestedUnionType__compute_offsets,
    (const char *)&data_NestedUnionType__compute_offsets.foo - (const char *)&data_NestedUnionType__compute_offsets,
    (const char *)&data_NestedUnionType__compute_offsets.bar - (const char *)&data_NestedUnionType__compute_offsets
};

static const ERD *data_NestedUnionType__childrenERDs[3] = {
    &_choice_data_NestedUnionType_ERD,
    &foo_data_NestedUnionType_ERD,
    &bar_data_NestedUnionType_ERD
};

static const ERD data_NestedUnionType_ERD = {
    {
        NULL, // namedQName.prefix
        "data", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX, // typeCode
    2, // numChildren
    data_NestedUnionType__offsets, // offsets
    data_NestedUnionType__childrenERDs, // childrenERDs
    (ERDParseSelf)&data_NestedUnionType__parseSelf, // parseSelf
    (ERDUnparseSelf)&data_NestedUnionType__unparseSelf, // unparseSelf
    (InitChoiceRD)&data_NestedUnionType__initChoice // initChoice
};

static const NestedUnion_ NestedUnion__compute_offsets;

static const size_t NestedUnion__offsets[2] = {
    (const char *)&NestedUnion__compute_offsets.tag - (const char *)&NestedUnion__compute_offsets,
    (const char *)&NestedUnion__compute_offsets.data - (const char *)&NestedUnion__compute_offsets
};

static const ERD *NestedUnion__childrenERDs[2] = {
    &tag_NestedUnionType_ERD,
    &data_NestedUnionType_ERD
};

static const ERD NestedUnion_ERD = {
    {
        "idl", // namedQName.prefix
        "NestedUnion", // namedQName.local
        "urn:idl:1.0", // namedQName.ns
    },
    COMPLEX, // typeCode
    2, // numChildren
    NestedUnion__offsets, // offsets
    NestedUnion__childrenERDs, // childrenERDs
    (ERDParseSelf)&NestedUnion__parseSelf, // parseSelf
    (ERDUnparseSelf)&NestedUnion__unparseSelf, // unparseSelf
    NULL // initChoice
};

// Initialize, parse, and unparse nodes of the infoset

static void
foo_data_NestedUnionType__initERD(foo_data_NestedUnionType_ *instance)
{
    instance->_base.erd = &foo_data_NestedUnionType_ERD;
}

static void
foo_data_NestedUnionType__initSelf(foo_data_NestedUnionType_ *instance)
{
    instance->a = 0x77777777;
    instance->b = 0x77777777;
    instance->c = 0x77777777;
}

static void
foo_data_NestedUnionType__parseSelf(foo_data_NestedUnionType_ *instance, PState *pstate)
{
    parse_be_int32(&instance->a, 32, pstate);
    if (pstate->error) return;
    parse_be_int32(&instance->b, 32, pstate);
    if (pstate->error) return;
    parse_be_int32(&instance->c, 32, pstate);
    if (pstate->error) return;
}

static void
foo_data_NestedUnionType__unparseSelf(const foo_data_NestedUnionType_ *instance, UState *ustate)
{
    unparse_be_int32(instance->a, 32, ustate);
    if (ustate->error) return;
    unparse_be_int32(instance->b, 32, ustate);
    if (ustate->error) return;
    unparse_be_int32(instance->c, 32, ustate);
    if (ustate->error) return;
}

static void
bar_data_NestedUnionType__initERD(bar_data_NestedUnionType_ *instance)
{
    instance->_base.erd = &bar_data_NestedUnionType_ERD;
}

static void
bar_data_NestedUnionType__initSelf(bar_data_NestedUnionType_ *instance)
{
    instance->x = NAN;
    instance->y = NAN;
    instance->z = NAN;
}

static void
bar_data_NestedUnionType__parseSelf(bar_data_NestedUnionType_ *instance, PState *pstate)
{
    parse_be_double(&instance->x, 64, pstate);
    if (pstate->error) return;
    parse_be_double(&instance->y, 64, pstate);
    if (pstate->error) return;
    parse_be_double(&instance->z, 64, pstate);
    if (pstate->error) return;
}

static void
bar_data_NestedUnionType__unparseSelf(const bar_data_NestedUnionType_ *instance, UState *ustate)
{
    unparse_be_double(instance->x, 64, ustate);
    if (ustate->error) return;
    unparse_be_double(instance->y, 64, ustate);
    if (ustate->error) return;
    unparse_be_double(instance->z, 64, ustate);
    if (ustate->error) return;
}

static void
data_NestedUnionType__initERD(data_NestedUnionType_ *instance)
{
    instance->_base.erd = &data_NestedUnionType_ERD;
}

static void
data_NestedUnionType__initSelf(data_NestedUnionType_ *instance)
{
    instance->_choice = 0xFFFFFFFFFFFFFFFF;
    foo_data_NestedUnionType__initSelf(&instance->foo);
    bar_data_NestedUnionType__initSelf(&instance->bar);
}

static const Error *
data_NestedUnionType__initChoice(data_NestedUnionType_ *instance, const NestedUnion_ *rootElement)
{
    static Error error = {ERR_CHOICE_KEY, {0}};

    int64_t key = rootElement->tag;
    switch (key)
    {
    case 1:
    case 2:
        instance->_choice = 0;
        foo_data_NestedUnionType__initERD(&instance->foo);
        break;
    case 3:
    case 4:
        instance->_choice = 1;
        bar_data_NestedUnionType__initERD(&instance->bar);
        break;
    default:
        error.arg.d64 = key;
        return &error;
    }

    return NULL;
}

static void
data_NestedUnionType__parseSelf(data_NestedUnionType_ *instance, PState *pstate)
{
    static Error error = {ERR_CHOICE_KEY, {0}};

    pstate->error = instance->_base.erd->initChoice(&instance->_base, rootElement());
    if (pstate->error) return;

    switch (instance->_choice)
    {
    case 0:
        foo_data_NestedUnionType__parseSelf(&instance->foo, pstate);
        if (pstate->error) return;
        break;
    case 1:
        bar_data_NestedUnionType__parseSelf(&instance->bar, pstate);
        if (pstate->error) return;
        break;
    default:
        // Should never happen because initChoice would return an error first
        error.arg.d64 = (int64_t)instance->_choice;
        pstate->error = &error;
        return;
    }
}

static void
data_NestedUnionType__unparseSelf(const data_NestedUnionType_ *instance, UState *ustate)
{
    static Error error = {ERR_CHOICE_KEY, {0}};

    ustate->error = instance->_base.erd->initChoice(&instance->_base, rootElement());
    if (ustate->error) return;

    switch (instance->_choice)
    {
    case 0:
        foo_data_NestedUnionType__unparseSelf(&instance->foo, ustate);
        if (ustate->error) return;
        break;
    case 1:
        bar_data_NestedUnionType__unparseSelf(&instance->bar, ustate);
        if (ustate->error) return;
        break;
    default:
        // Should never happen because initChoice would return an error first
        error.arg.d64 = (int64_t)instance->_choice;
        ustate->error = &error;
        return;
    }
}

static void
NestedUnion__initERD(NestedUnion_ *instance)
{
    instance->_base.erd = &NestedUnion_ERD;
    data_NestedUnionType__initERD(&instance->data);
}

static void
NestedUnion__initSelf(NestedUnion_ *instance)
{
    instance->tag = 0x77777777;
    data_NestedUnionType__initSelf(&instance->data);
}

static void
NestedUnion__parseSelf(NestedUnion_ *instance, PState *pstate)
{
    parse_be_int32(&instance->tag, 32, pstate);
    if (pstate->error) return;
    data_NestedUnionType__parseSelf(&instance->data, pstate);
    if (pstate->error) return;
}

static void
NestedUnion__unparseSelf(const NestedUnion_ *instance, UState *ustate)
{
    unparse_be_int32(instance->tag, 32, ustate);
    if (ustate->error) return;
    data_NestedUnionType__unparseSelf(&instance->data, ustate);
    if (ustate->error) return;
}

// Return a root element for parsing or unparsing the infoset

InfosetBase *
rootElement(void)
{
    static bool initialized;
    static NestedUnion_ root;
    if (!initialized)
    {
        NestedUnion__initERD(&root);
        NestedUnion__initSelf(&root);
        initialized = true;
    }
    return &root._base;
}
