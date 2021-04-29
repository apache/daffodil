#include "generated_code.h"
#include <math.h>       // for NAN
#include <stdbool.h>    // for bool, true, false
#include <stddef.h>     // for NULL, size_t
#include "errors.h"     // for Error, PState, UState, ERR_CHOICE_KEY, UNUSED
#include "parsers.h"    // for parse_be_float, parse_be_int16, parse_be_bool32, parse_validate_fixed, parse_be_bool16, parse_be_int32, parse_be_uint32, parse_le_bool32, parse_le_int64, parse_le_uint8, parse_be_bool8, parse_be_double, parse_be_int64, parse_be_int8, parse_be_uint16, parse_be_uint64, parse_be_uint8, parse_le_bool16, parse_le_bool8, parse_le_double, parse_le_float, parse_le_int16, parse_le_int32, parse_le_int8, parse_le_uint16, parse_le_uint32, parse_le_uint64
#include "unparsers.h"  // for unparse_be_float, unparse_be_int16, unparse_be_bool32, unparse_validate_fixed, unparse_be_bool16, unparse_be_int32, unparse_be_uint32, unparse_le_bool32, unparse_le_int64, unparse_le_uint8, unparse_be_bool8, unparse_be_double, unparse_be_int64, unparse_be_int8, unparse_be_uint16, unparse_be_uint64, unparse_be_uint8, unparse_le_bool16, unparse_le_bool8, unparse_le_double, unparse_le_float, unparse_le_int16, unparse_le_int32, unparse_le_int8, unparse_le_uint16, unparse_le_uint32, unparse_le_uint64

// Initialize our program's name and version

const char *daffodil_program_version = "daffodil-runtime2 3.1.0-SNAPSHOT";

// Declare prototypes for easier compilation

static void foo_initSelf(foo *instance);
static void foo_parseSelf(foo *instance, PState *pstate);
static void foo_unparseSelf(const foo *instance, UState *ustate);
static void bar_initSelf(bar *instance);
static void bar_parseSelf(bar *instance, PState *pstate);
static void bar_unparseSelf(const bar *instance, UState *ustate);
static void data_initSelf(data *instance);
static const Error *data_initChoice(data *instance, const NestedUnion *rootElement);
static void data_parseSelf(data *instance, PState *pstate);
static void data_unparseSelf(const data *instance, UState *ustate);
static void NestedUnion_initSelf(NestedUnion *instance);
static void NestedUnion_parseSelf(NestedUnion *instance, PState *pstate);
static void NestedUnion_unparseSelf(const NestedUnion *instance, UState *ustate);

// Define metadata for the infoset

static const ERD tag_NestedUnionType_ERD = {
    {
        NULL, // namedQName.prefix
        "tag", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL, NULL
};

static const ERD _choice_data_NestedUnionType_ERD = {
    {
        NULL, // namedQName.prefix
        "_choice", // namedQName.local
        NULL, // namedQName.ns
    },
    CHOICE, // typeCode
    0, NULL, NULL, NULL, NULL, NULL, NULL
};

static const ERD a_FooType_ERD = {
    {
        NULL, // namedQName.prefix
        "a", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL, NULL
};

static const ERD b_FooType_ERD = {
    {
        NULL, // namedQName.prefix
        "b", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL, NULL
};

static const ERD c_FooType_ERD = {
    {
        NULL, // namedQName.prefix
        "c", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL, NULL
};

static const foo foo_compute_offsets;

static const size_t foo_offsets[3] = {
    (const char *)&foo_compute_offsets.a - (const char *)&foo_compute_offsets,
    (const char *)&foo_compute_offsets.b - (const char *)&foo_compute_offsets,
    (const char *)&foo_compute_offsets.c - (const char *)&foo_compute_offsets
};

static const ERD *foo_childrenERDs[3] = {
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
    foo_offsets, // offsets
    foo_childrenERDs, // childrenERDs
    (ERDInitSelf)&foo_initSelf, // initSelf
    (ERDParseSelf)&foo_parseSelf, // parseSelf
    (ERDUnparseSelf)&foo_unparseSelf, // unparseSelf
    NULL // initChoice
};

static const ERD x_BarType_ERD = {
    {
        NULL, // namedQName.prefix
        "x", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, NULL, NULL
};

static const ERD y_BarType_ERD = {
    {
        NULL, // namedQName.prefix
        "y", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, NULL, NULL
};

static const ERD z_BarType_ERD = {
    {
        NULL, // namedQName.prefix
        "z", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, NULL, NULL
};

static const bar bar_compute_offsets;

static const size_t bar_offsets[3] = {
    (const char *)&bar_compute_offsets.x - (const char *)&bar_compute_offsets,
    (const char *)&bar_compute_offsets.y - (const char *)&bar_compute_offsets,
    (const char *)&bar_compute_offsets.z - (const char *)&bar_compute_offsets
};

static const ERD *bar_childrenERDs[3] = {
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
    bar_offsets, // offsets
    bar_childrenERDs, // childrenERDs
    (ERDInitSelf)&bar_initSelf, // initSelf
    (ERDParseSelf)&bar_parseSelf, // parseSelf
    (ERDUnparseSelf)&bar_unparseSelf, // unparseSelf
    NULL // initChoice
};

static const data data_compute_offsets;

static const size_t data_offsets[3] = {
    (const char *)&data_compute_offsets._choice - (const char *)&data_compute_offsets,
    (const char *)&data_compute_offsets.foo - (const char *)&data_compute_offsets,
    (const char *)&data_compute_offsets.bar - (const char *)&data_compute_offsets
};

static const ERD *data_childrenERDs[3] = {
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
    data_offsets, // offsets
    data_childrenERDs, // childrenERDs
    (ERDInitSelf)&data_initSelf, // initSelf
    (ERDParseSelf)&data_parseSelf, // parseSelf
    (ERDUnparseSelf)&data_unparseSelf, // unparseSelf
    (InitChoiceRD)&data_initChoice // initChoice
};

static const NestedUnion NestedUnion_compute_offsets;

static const size_t NestedUnion_offsets[2] = {
    (const char *)&NestedUnion_compute_offsets.tag - (const char *)&NestedUnion_compute_offsets,
    (const char *)&NestedUnion_compute_offsets.data - (const char *)&NestedUnion_compute_offsets
};

static const ERD *NestedUnion_childrenERDs[2] = {
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
    NestedUnion_offsets, // offsets
    NestedUnion_childrenERDs, // childrenERDs
    (ERDInitSelf)&NestedUnion_initSelf, // initSelf
    (ERDParseSelf)&NestedUnion_parseSelf, // parseSelf
    (ERDUnparseSelf)&NestedUnion_unparseSelf, // unparseSelf
    NULL // initChoice
};

// Return a root element for parsing or unparsing the infoset

InfosetBase *
rootElement(void)
{
    static bool initialized;
    static NestedUnion root;
    if (!initialized)
    {
        NestedUnion_initSelf(&root);
        initialized = true;
    }
    return &root._base;
}

// Initialize, parse, and unparse nodes of the infoset

static void
foo_initSelf(foo *instance)
{
    instance->_base.erd = &foo_data_NestedUnionType_ERD;
    instance->a = 0xCCCCCCCC;
    instance->b = 0xCCCCCCCC;
    instance->c = 0xCCCCCCCC;
}

static void
foo_parseSelf(foo *instance, PState *pstate)
{
    parse_be_int32(&instance->a, pstate);
    if (pstate->error) return;
    parse_be_int32(&instance->b, pstate);
    if (pstate->error) return;
    parse_be_int32(&instance->c, pstate);
    if (pstate->error) return;
}

static void
foo_unparseSelf(const foo *instance, UState *ustate)
{
    unparse_be_int32(instance->a, ustate);
    if (ustate->error) return;
    unparse_be_int32(instance->b, ustate);
    if (ustate->error) return;
    unparse_be_int32(instance->c, ustate);
    if (ustate->error) return;
}

static void
bar_initSelf(bar *instance)
{
    instance->_base.erd = &bar_data_NestedUnionType_ERD;
    instance->x = NAN;
    instance->y = NAN;
    instance->z = NAN;
}

static void
bar_parseSelf(bar *instance, PState *pstate)
{
    parse_be_double(&instance->x, pstate);
    if (pstate->error) return;
    parse_be_double(&instance->y, pstate);
    if (pstate->error) return;
    parse_be_double(&instance->z, pstate);
    if (pstate->error) return;
}

static void
bar_unparseSelf(const bar *instance, UState *ustate)
{
    unparse_be_double(instance->x, ustate);
    if (ustate->error) return;
    unparse_be_double(instance->y, ustate);
    if (ustate->error) return;
    unparse_be_double(instance->z, ustate);
    if (ustate->error) return;
}

static void
data_initSelf(data *instance)
{
    instance->_base.erd = &data_NestedUnionType_ERD;
    instance->_choice = 0xFFFFFFFFFFFFFFFF;
    foo_initSelf(&instance->foo);
    bar_initSelf(&instance->bar);
}

static const Error *
data_initChoice(data *instance, const NestedUnion *rootElement)
{
    static Error error = {ERR_CHOICE_KEY, {0}};

    int64_t key = rootElement->tag;
    switch (key)
    {
    case 1:
    case 2:
        instance->_choice = 0;
        break;
    case 3:
    case 4:
        instance->_choice = 1;
        break;
    default:
        error.d64 = key;
        return &error;
    }

    // Point next ERD to choice of alternative elements' ERDs
    const size_t choice = instance->_choice + 1; // skip the _choice field
    const size_t offset = instance->_base.erd->offsets[choice];
    const ERD *  childERD = instance->_base.erd->childrenERDs[choice];
    InfosetBase *childNode = (InfosetBase *)((const char *)instance + offset);
    childNode->erd = childERD;

    return NULL;
}

static void
data_parseSelf(data *instance, PState *pstate)
{
    static Error error = {ERR_CHOICE_KEY, {0}};

    pstate->error = instance->_base.erd->initChoice(&instance->_base, rootElement());
    if (pstate->error) return;

    switch (instance->_choice)
    {
    case 0:
        foo_parseSelf(&instance->foo, pstate);
        if (pstate->error) return;
        break;
    case 1:
        bar_parseSelf(&instance->bar, pstate);
        if (pstate->error) return;
        break;
    default:
        // Should never happen because initChoice would return an error first
        error.d64 = (int64_t)instance->_choice;
        pstate->error = &error;
        return;
    }
}

static void
data_unparseSelf(const data *instance, UState *ustate)
{
    static Error error = {ERR_CHOICE_KEY, {0}};

    ustate->error = instance->_base.erd->initChoice(&instance->_base, rootElement());
    if (ustate->error) return;

    switch (instance->_choice)
    {
    case 0:
        foo_unparseSelf(&instance->foo, ustate);
        if (ustate->error) return;
        break;
    case 1:
        bar_unparseSelf(&instance->bar, ustate);
        if (ustate->error) return;
        break;
    default:
        // Should never happen because initChoice would return an error first
        error.d64 = (int64_t)instance->_choice;
        ustate->error = &error;
        return;
    }
}

static void
NestedUnion_initSelf(NestedUnion *instance)
{
    instance->_base.erd = &NestedUnion_ERD;
    instance->tag = 0xCCCCCCCC;
    data_initSelf(&instance->data);
}

static void
NestedUnion_parseSelf(NestedUnion *instance, PState *pstate)
{
    parse_be_int32(&instance->tag, pstate);
    if (pstate->error) return;
    data_parseSelf(&instance->data, pstate);
    if (pstate->error) return;
}

static void
NestedUnion_unparseSelf(const NestedUnion *instance, UState *ustate)
{
    unparse_be_int32(instance->tag, ustate);
    if (ustate->error) return;
    data_unparseSelf(&instance->data, ustate);
    if (ustate->error) return;
}

