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

static void padhexbinary_padtest__parseSelf(padhexbinary_padtest_ *instance, PState *pstate);
static void padhexbinary_padtest__unparseSelf(const padhexbinary_padtest_ *instance, UState *ustate);
static void padtest__parseSelf(padtest_ *instance, PState *pstate);
static void padtest__unparseSelf(const padtest_ *instance, UState *ustate);

// Define metadata for the infoset

static const ERD opaque_padhexbinary_ERD = {
    {
        NULL, // namedQName.prefix
        "opaque", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const padhexbinary_padtest_ padhexbinary_padtest__compute_offsets;

static const size_t padhexbinary_padtest__offsets[1] = {
    (const char *)&padhexbinary_padtest__compute_offsets.opaque - (const char *)&padhexbinary_padtest__compute_offsets
};

static const ERD *const padhexbinary_padtest__childrenERDs[1] = {
    &opaque_padhexbinary_ERD
};

static const ERD padhexbinary_padtest_ERD = {
    {
        NULL, // namedQName.prefix
        "padhexbinary", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX, // typeCode
    1, // numChildren
    padhexbinary_padtest__offsets, // offsets
    padhexbinary_padtest__childrenERDs, // childrenERDs
    (ERDParseSelf)&padhexbinary_padtest__parseSelf, // parseSelf
    (ERDUnparseSelf)&padhexbinary_padtest__unparseSelf, // unparseSelf
    {NULL} // initChoice
};

static const ERD after_padtest_ERD = {
    {
        NULL, // namedQName.prefix
        "after", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const padtest_ padtest__compute_offsets;

static const size_t padtest__offsets[2] = {
    (const char *)&padtest__compute_offsets.padhexbinary - (const char *)&padtest__compute_offsets,
    (const char *)&padtest__compute_offsets.after - (const char *)&padtest__compute_offsets
};

static const ERD *const padtest__childrenERDs[2] = {
    &padhexbinary_padtest_ERD,
    &after_padtest_ERD
};

static const ERD padtest_ERD = {
    {
        NULL, // namedQName.prefix
        "padtest", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX, // typeCode
    2, // numChildren
    padtest__offsets, // offsets
    padtest__childrenERDs, // childrenERDs
    (ERDParseSelf)&padtest__parseSelf, // parseSelf
    (ERDUnparseSelf)&padtest__unparseSelf, // unparseSelf
    {NULL} // initChoice
};

// Initialize, parse, and unparse nodes of the infoset

static void
padhexbinary_padtest__initERD(padhexbinary_padtest_ *instance, InfosetBase *parent)
{
    instance->_base.erd = &padhexbinary_padtest_ERD;
    instance->_base.parent = parent;
    instance->opaque.dynamic = true;
}

static void
padhexbinary_padtest__parseSelf(padhexbinary_padtest_ *instance, PState *pstate)
{
    uint32_t _l_opaque;
    parse_be_uint32(&_l_opaque, 32, pstate);
    if (pstate->error) return;
    alloc_hexBinary(&instance->opaque, _l_opaque, pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->opaque, pstate);
    if (pstate->error) return;
    // Fill to closest alignment
    parse_align(32, pstate);
    if (pstate->error) return;
}

static void
padhexbinary_padtest__unparseSelf(const padhexbinary_padtest_ *instance, UState *ustate)
{
    unparse_be_uint32(instance->opaque.lengthInBytes, 32, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->opaque, ustate);
    if (ustate->error) return;
    // Fill to closest alignment
    unparse_align(32, '\0', ustate);
    if (ustate->error) return;
}

static void
padtest__initERD(padtest_ *instance, InfosetBase *parent)
{
    instance->_base.erd = &padtest_ERD;
    instance->_base.parent = parent;
    padhexbinary_padtest__initERD(&instance->padhexbinary, (InfosetBase *)instance);
}

static void
padtest__parseSelf(padtest_ *instance, PState *pstate)
{
    padhexbinary_padtest__parseSelf(&instance->padhexbinary, pstate);
    if (pstate->error) return;
    parse_be_uint32(&instance->after, 32, pstate);
    if (pstate->error) return;
}

static void
padtest__unparseSelf(const padtest_ *instance, UState *ustate)
{
    padhexbinary_padtest__unparseSelf(&instance->padhexbinary, ustate);
    if (ustate->error) return;
    unparse_be_uint32(instance->after, 32, ustate);
    if (ustate->error) return;
}

// Return a root element for parsing or unparsing the infoset

InfosetBase *
rootElement(void)
{
    static bool initialized;
    static padtest_ root;
    if (!initialized)
    {
        padtest__initERD(&root, (InfosetBase *)&root);
        initialized = true;
    }
    return &root._base;
}
