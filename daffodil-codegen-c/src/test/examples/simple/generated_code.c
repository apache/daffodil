// clang-format off
#include "generated_code.h"
#include <stdbool.h>    // for false, bool, true
#include <stddef.h>     // for NULL, size_t
#include <string.h>     // for memcmp, memset
#include "errors.h"     // for Error, PState, UState, ERR_CHOICE_KEY, Error::(anonymous), UNUSED
#include "parsers.h"    // for alloc_hexBinary, parse_hexBinary, parse_be_float, parse_be_int16, parse_validate_fixed, parse_be_bool32, parse_be_bool16, parse_be_int32, parse_be_uint16, parse_be_uint32, parse_le_bool32, parse_le_int64, parse_le_uint16, parse_le_uint8, parse_be_bool8, parse_be_double, parse_be_int64, parse_be_int8, parse_be_uint64, parse_be_uint8, parse_le_bool16, parse_le_bool8, parse_le_double, parse_le_float, parse_le_int16, parse_le_int32, parse_le_int8, parse_le_uint32, parse_le_uint64
#include "unparsers.h"  // for unparse_hexBinary, unparse_be_float, unparse_be_int16, unparse_validate_fixed, unparse_be_bool32, unparse_be_bool16, unparse_be_int32, unparse_be_uint16, unparse_be_uint32, unparse_le_bool32, unparse_le_int64, unparse_le_uint16, unparse_le_uint8, unparse_be_bool8, unparse_be_double, unparse_be_int64, unparse_be_int8, unparse_be_uint64, unparse_be_uint8, unparse_le_bool16, unparse_le_bool8, unparse_le_double, unparse_le_float, unparse_le_int16, unparse_le_int32, unparse_le_int8, unparse_le_uint32, unparse_le_uint64
// clang-format on

// Declare prototypes for easier compilation

static void simple_byte__parseSelf(simple_byte_ *instance, PState *pstate);
static void simple_byte__unparseSelf(const simple_byte_ *instance, UState *ustate);

// Define metadata for the infoset

static const simple_byte_ simple_byte__compute_offsets;

static const size_t simple_byte__childrenOffsets[1] = {
    (const char *)&simple_byte__compute_offsets.simple_byte - (const char *)&simple_byte__compute_offsets
};

static const ERD simple_byte_ERD = {
    {
        "si", // namedQName.prefix
        "simple-byte", // namedQName.local
        "urn:simple", // namedQName.ns
    },
    PRIMITIVE_INT8, // typeCode
    0, // numChildren
    simple_byte__childrenOffsets,
    NULL, // childrenERDs
    (ERDParseSelf)&simple_byte__parseSelf,
    (ERDUnparseSelf)&simple_byte__unparseSelf,
    {.initChoice = NULL}
};

// Initialize, parse, and unparse nodes of the infoset

static void
simple_byte__initERD(simple_byte_ *instance, InfosetBase *parent)
{
    instance->_base.erd = &simple_byte_ERD;
    instance->_base.parent = parent;
}

static void
simple_byte__parseSelf(simple_byte_ *instance, PState *pstate)
{
    parse_be_int8(&instance->simple_byte, 8, pstate);
    if (pstate->error) return;
}

static void
simple_byte__unparseSelf(const simple_byte_ *instance, UState *ustate)
{
    unparse_be_int8(instance->simple_byte, 8, ustate);
    if (ustate->error) return;
}

// Get an infoset (optionally clearing it first) for parsing/walking

InfosetBase *
get_infoset(bool clear_infoset)
{
    static simple_byte_ infoset;

    if (clear_infoset)
    {
        // If your infoset contains hexBinary prefixed length elements,
        // you may want to walk infoset first to free their malloc'ed
        // storage - we are not handling that case for now...
        memset(&infoset, 0, sizeof(infoset));
        simple_byte__initERD(&infoset, (InfosetBase *)&infoset);
    }

    return &infoset._base;
}
