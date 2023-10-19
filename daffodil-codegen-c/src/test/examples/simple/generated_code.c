// auto-maintained by iwyu
// clang-format off
#include "generated_code.h"
#include <stdbool.h>    // for false, bool, true
#include <stddef.h>     // for NULL, size_t
#include <string.h>     // for memcmp, memset
#include "errors.h"     // for Error, PState, UState, ERR_CHOICE_KEY, Error::(anonymous), UNUSED
#include "parsers.h"    // for alloc_hexBinary, parse_hexBinary, parse_be_float, parse_be_int16, parse_be_bool32, parse_be_bool16, parse_be_int32, parse_be_uint16, parse_be_uint32, parse_le_bool32, parse_le_int64, parse_le_uint16, parse_le_uint8, parse_be_bool8, parse_be_double, parse_be_int64, parse_be_int8, parse_be_uint64, parse_be_uint8, parse_le_bool16, parse_le_bool8, parse_le_double, parse_le_float, parse_le_int16, parse_le_int32, parse_le_int8, parse_le_uint32, parse_le_uint64
#include "unparsers.h"  // for unparse_hexBinary, unparse_be_float, unparse_be_int16, unparse_be_bool32, unparse_be_bool16, unparse_be_int32, unparse_be_uint16, unparse_be_uint32, unparse_le_bool32, unparse_le_int64, unparse_le_uint16, unparse_le_uint8, unparse_be_bool8, unparse_be_double, unparse_be_int64, unparse_be_int8, unparse_be_uint64, unparse_be_uint8, unparse_le_bool16, unparse_le_bool8, unparse_le_double, unparse_le_float, unparse_le_int16, unparse_le_int32, unparse_le_int8, unparse_le_uint32, unparse_le_uint64
#include "validators.h" // for validate_array_bounds, validate_fixed_attribute, validate_floatpt_enumeration, validate_integer_enumeration, validate_schema_range
// clang-format on

// Declare prototypes for easier compilation

static void simple__parseSelf(simple_ *instance, PState *pstate);
static void simple__unparseSelf(const simple_ *instance, UState *ustate);

// Define schema version (will be empty if schema did not define any version string)

const char *schema_version = "2.1.1";

// Define metadata for the infoset

static const ERD simple_boolean_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-boolean", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_byte_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-byte", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT8, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_double_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-double", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_float_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-float", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_hexBinary_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-hexBinary", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_hexBinaryPrefixed_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-hexBinaryPrefixed", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_int_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-int", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_integer_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-integer", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_long_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-long", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_nonNegativeInteger_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-nonNegativeInteger", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_short_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-short", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_unsignedByte_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-unsignedByte", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT8, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_unsignedInt_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-unsignedInt", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_unsignedLong_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-unsignedLong", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT64, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD simple_unsignedShort_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "simple-unsignedShort", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_byte_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-byte", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT8, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_double_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-double", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_float_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-float", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_hexBinary_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-hexBinary", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_hexBinaryPrefixed_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-hexBinaryPrefixed", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_int_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-int", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_integer_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-integer", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_long_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-long", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_nonNegativeInteger_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-nonNegativeInteger", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_short_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-short", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_unsignedByte_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-unsignedByte", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT8, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_unsignedInt_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-unsignedInt", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_unsignedLong_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-unsignedLong", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT64, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD enum_unsignedShort_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "enum-unsignedShort", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD range_byte_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "range-byte", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT8, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD range_double_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "range-double", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD range_float_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "range-float", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD range_int_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "range-int", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD range_integer_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "range-integer", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD range_long_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "range-long", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD range_nonNegativeInteger_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "range-nonNegativeInteger", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD range_short_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "range-short", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD range_unsignedByte_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "range-unsignedByte", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT8, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD range_unsignedInt_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "range-unsignedInt", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD range_unsignedLong_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "range-unsignedLong", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT64, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD range_unsignedShort_simple_ERD = {
    {
        NULL, // namedQName.prefix
        "range-unsignedShort", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const simple_ simple__compute_offsets;

static const size_t simple__childrenOffsets[41] = {
    (const char *)&simple__compute_offsets.simple_boolean - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_byte - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_double - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_float - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_hexBinary - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_hexBinaryPrefixed - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_int - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_integer - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_long - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_nonNegativeInteger - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_short - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_unsignedByte - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_unsignedInt - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_unsignedLong - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.simple_unsignedShort - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_byte - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_double - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_float - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_hexBinary - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_hexBinaryPrefixed - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_int - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_integer - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_long - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_nonNegativeInteger - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_short - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_unsignedByte - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_unsignedInt - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_unsignedLong - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.enum_unsignedShort - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.range_byte - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.range_double - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.range_float - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.range_int - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.range_integer - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.range_long - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.range_nonNegativeInteger - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.range_short - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.range_unsignedByte - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.range_unsignedInt - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.range_unsignedLong - (const char *)&simple__compute_offsets,
    (const char *)&simple__compute_offsets.range_unsignedShort - (const char *)&simple__compute_offsets
};

static const ERD *const simple__childrenERDs[41] = {
    &simple_boolean_simple_ERD,
    &simple_byte_simple_ERD,
    &simple_double_simple_ERD,
    &simple_float_simple_ERD,
    &simple_hexBinary_simple_ERD,
    &simple_hexBinaryPrefixed_simple_ERD,
    &simple_int_simple_ERD,
    &simple_integer_simple_ERD,
    &simple_long_simple_ERD,
    &simple_nonNegativeInteger_simple_ERD,
    &simple_short_simple_ERD,
    &simple_unsignedByte_simple_ERD,
    &simple_unsignedInt_simple_ERD,
    &simple_unsignedLong_simple_ERD,
    &simple_unsignedShort_simple_ERD,
    &enum_byte_simple_ERD,
    &enum_double_simple_ERD,
    &enum_float_simple_ERD,
    &enum_hexBinary_simple_ERD,
    &enum_hexBinaryPrefixed_simple_ERD,
    &enum_int_simple_ERD,
    &enum_integer_simple_ERD,
    &enum_long_simple_ERD,
    &enum_nonNegativeInteger_simple_ERD,
    &enum_short_simple_ERD,
    &enum_unsignedByte_simple_ERD,
    &enum_unsignedInt_simple_ERD,
    &enum_unsignedLong_simple_ERD,
    &enum_unsignedShort_simple_ERD,
    &range_byte_simple_ERD,
    &range_double_simple_ERD,
    &range_float_simple_ERD,
    &range_int_simple_ERD,
    &range_integer_simple_ERD,
    &range_long_simple_ERD,
    &range_nonNegativeInteger_simple_ERD,
    &range_short_simple_ERD,
    &range_unsignedByte_simple_ERD,
    &range_unsignedInt_simple_ERD,
    &range_unsignedLong_simple_ERD,
    &range_unsignedShort_simple_ERD
};

static const ERD simple_ERD = {
    {
        "si", // namedQName.prefix
        "simple", // namedQName.local
        "urn:simple", // namedQName.ns
    },
    COMPLEX, // typeCode
    41, // numChildren
    simple__childrenOffsets,
    simple__childrenERDs,
    (ERDParseSelf)&simple__parseSelf,
    (ERDUnparseSelf)&simple__unparseSelf,
    {.initChoice = NULL}
};

// Initialize, parse, and unparse nodes of the infoset

static void
simple__initERD(simple_ *instance, InfosetBase *parent)
{
    instance->_base.erd = &simple_ERD;
    instance->_base.parent = parent;
    instance->simple_hexBinary.array = instance->_a_simple_hexBinary;
    instance->simple_hexBinary.lengthInBytes = sizeof(instance->_a_simple_hexBinary);
    instance->simple_hexBinary.dynamic = false;
    instance->simple_hexBinaryPrefixed.dynamic = true;
    instance->enum_hexBinary.array = instance->_a_enum_hexBinary;
    instance->enum_hexBinary.lengthInBytes = sizeof(instance->_a_enum_hexBinary);
    instance->enum_hexBinary.dynamic = false;
    instance->enum_hexBinaryPrefixed.dynamic = true;
}

static void
simple__parseSelf(simple_ *instance, PState *pstate)
{
    parse_be_bool(&instance->simple_boolean, 32, 1, 0, pstate);
    if (pstate->pu.error) return;
    parse_be_int8(&instance->simple_byte, 8, pstate);
    if (pstate->pu.error) return;
    parse_be_double(&instance->simple_double, 64, pstate);
    if (pstate->pu.error) return;
    parse_be_float(&instance->simple_float, 32, pstate);
    if (pstate->pu.error) return;
    parse_hexBinary(&instance->simple_hexBinary, pstate);
    if (pstate->pu.error) return;
    uint16_t _l_simple_hexBinaryPrefixed;
    parse_be_uint16(&_l_simple_hexBinaryPrefixed, 16, pstate);
    if (pstate->pu.error) return;
    alloc_hexBinary(&instance->simple_hexBinaryPrefixed, _l_simple_hexBinaryPrefixed, pstate);
    if (pstate->pu.error) return;
    parse_hexBinary(&instance->simple_hexBinaryPrefixed, pstate);
    if (pstate->pu.error) return;
    parse_be_int32(&instance->simple_int, 32, pstate);
    if (pstate->pu.error) return;
    parse_be_int32(&instance->simple_integer, 32, pstate);
    if (pstate->pu.error) return;
    parse_be_int64(&instance->simple_long, 64, pstate);
    if (pstate->pu.error) return;
    parse_be_uint32(&instance->simple_nonNegativeInteger, 32, pstate);
    if (pstate->pu.error) return;
    parse_be_int16(&instance->simple_short, 16, pstate);
    if (pstate->pu.error) return;
    parse_be_uint8(&instance->simple_unsignedByte, 8, pstate);
    if (pstate->pu.error) return;
    parse_be_uint32(&instance->simple_unsignedInt, 32, pstate);
    if (pstate->pu.error) return;
    parse_be_uint64(&instance->simple_unsignedLong, 64, pstate);
    if (pstate->pu.error) return;
    parse_be_uint16(&instance->simple_unsignedShort, 16, pstate);
    if (pstate->pu.error) return;
    parse_be_int8(&instance->enum_byte, 8, pstate);
    if (pstate->pu.error) return;
    int64_t enums_enum_byte[] = {1, 2};
    validate_integer_enumeration(instance->enum_byte, 2, enums_enum_byte, "enum_byte", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_double(&instance->enum_double, 64, pstate);
    if (pstate->pu.error) return;
    double enums_enum_double[] = {1.0, 2.0};
    validate_floatpt_enumeration(instance->enum_double, 2, enums_enum_double, "enum_double", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_float(&instance->enum_float, 32, pstate);
    if (pstate->pu.error) return;
    double enums_enum_float[] = {1.0, 2.0};
    validate_floatpt_enumeration(instance->enum_float, 2, enums_enum_float, "enum_float", &pstate->pu);
    if (pstate->pu.error) return;
    parse_hexBinary(&instance->enum_hexBinary, pstate);
    if (pstate->pu.error) return;
    uint8_t arrays_enum_hexBinary[][4] = {{0x11, 0x22, 0x33, 0x44}, {0xAA, 0xBB, 0xCC, 0xDD}};
    HexBinary enums_enum_hexBinary[] = {{arrays_enum_hexBinary[0], 4, false}, {arrays_enum_hexBinary[1], 4, false}};
    validate_hexbinary_enumeration(&instance->enum_hexBinary, 2, enums_enum_hexBinary, "enum_hexBinary", &pstate->pu);
    if (pstate->pu.error) return;
    uint16_t _l_enum_hexBinaryPrefixed;
    parse_be_uint16(&_l_enum_hexBinaryPrefixed, 16, pstate);
    if (pstate->pu.error) return;
    alloc_hexBinary(&instance->enum_hexBinaryPrefixed, _l_enum_hexBinaryPrefixed, pstate);
    if (pstate->pu.error) return;
    parse_hexBinary(&instance->enum_hexBinaryPrefixed, pstate);
    if (pstate->pu.error) return;
    uint8_t arrays_enum_hexBinaryPrefixed[][4] = {{0x11, 0x22, 0x33, 0x44}, {0xAA, 0xBB, 0xCC, 0xDD}};
    HexBinary enums_enum_hexBinaryPrefixed[] = {{arrays_enum_hexBinaryPrefixed[0], 4, false}, {arrays_enum_hexBinaryPrefixed[1], 4, false}};
    validate_hexbinary_enumeration(&instance->enum_hexBinaryPrefixed, 2, enums_enum_hexBinaryPrefixed, "enum_hexBinaryPrefixed", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_int32(&instance->enum_int, 32, pstate);
    if (pstate->pu.error) return;
    int64_t enums_enum_int[] = {1, 2};
    validate_integer_enumeration(instance->enum_int, 2, enums_enum_int, "enum_int", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_int32(&instance->enum_integer, 32, pstate);
    if (pstate->pu.error) return;
    int64_t enums_enum_integer[] = {1, 2};
    validate_integer_enumeration(instance->enum_integer, 2, enums_enum_integer, "enum_integer", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_int64(&instance->enum_long, 64, pstate);
    if (pstate->pu.error) return;
    int64_t enums_enum_long[] = {1, 2};
    validate_integer_enumeration(instance->enum_long, 2, enums_enum_long, "enum_long", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_uint32(&instance->enum_nonNegativeInteger, 32, pstate);
    if (pstate->pu.error) return;
    int64_t enums_enum_nonNegativeInteger[] = {1, 2};
    validate_integer_enumeration(instance->enum_nonNegativeInteger, 2, enums_enum_nonNegativeInteger, "enum_nonNegativeInteger", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_int16(&instance->enum_short, 16, pstate);
    if (pstate->pu.error) return;
    int64_t enums_enum_short[] = {1, 2};
    validate_integer_enumeration(instance->enum_short, 2, enums_enum_short, "enum_short", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_uint8(&instance->enum_unsignedByte, 8, pstate);
    if (pstate->pu.error) return;
    int64_t enums_enum_unsignedByte[] = {1, 2};
    validate_integer_enumeration(instance->enum_unsignedByte, 2, enums_enum_unsignedByte, "enum_unsignedByte", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_uint32(&instance->enum_unsignedInt, 32, pstate);
    if (pstate->pu.error) return;
    int64_t enums_enum_unsignedInt[] = {1, 2};
    validate_integer_enumeration(instance->enum_unsignedInt, 2, enums_enum_unsignedInt, "enum_unsignedInt", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_uint64(&instance->enum_unsignedLong, 64, pstate);
    if (pstate->pu.error) return;
    int64_t enums_enum_unsignedLong[] = {1, 2};
    validate_integer_enumeration(instance->enum_unsignedLong, 2, enums_enum_unsignedLong, "enum_unsignedLong", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_uint16(&instance->enum_unsignedShort, 16, pstate);
    if (pstate->pu.error) return;
    int64_t enums_enum_unsignedShort[] = {1, 2};
    validate_integer_enumeration(instance->enum_unsignedShort, 2, enums_enum_unsignedShort, "enum_unsignedShort", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_int8(&instance->range_byte, 8, pstate);
    if (pstate->pu.error) return;
    validate_schema_range(instance->range_byte > 0 && instance->range_byte <= 100, "range_byte", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_double(&instance->range_double, 64, pstate);
    if (pstate->pu.error) return;
    validate_schema_range(instance->range_double >= 1.0 && instance->range_double <= 2.0, "range_double", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_float(&instance->range_float, 32, pstate);
    if (pstate->pu.error) return;
    validate_schema_range(instance->range_float > 0.0 && instance->range_float < 1.0, "range_float", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_int32(&instance->range_int, 32, pstate);
    if (pstate->pu.error) return;
    validate_schema_range(instance->range_int >= 1 && instance->range_int < 2, "range_int", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_int32(&instance->range_integer, 32, pstate);
    if (pstate->pu.error) return;
    validate_schema_range(instance->range_integer >= 1 && instance->range_integer <= 2, "range_integer", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_int64(&instance->range_long, 64, pstate);
    if (pstate->pu.error) return;
    validate_schema_range(instance->range_long >= 1 && instance->range_long <= 2, "range_long", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_uint32(&instance->range_nonNegativeInteger, 32, pstate);
    if (pstate->pu.error) return;
    validate_schema_range(instance->range_nonNegativeInteger >= 1 && instance->range_nonNegativeInteger <= 2, "range_nonNegativeInteger", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_int16(&instance->range_short, 16, pstate);
    if (pstate->pu.error) return;
    validate_schema_range(instance->range_short >= 1 && instance->range_short <= 2, "range_short", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_uint8(&instance->range_unsignedByte, 8, pstate);
    if (pstate->pu.error) return;
    validate_schema_range(instance->range_unsignedByte >= 1 && instance->range_unsignedByte <= 2, "range_unsignedByte", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_uint32(&instance->range_unsignedInt, 32, pstate);
    if (pstate->pu.error) return;
    validate_schema_range(instance->range_unsignedInt >= 1 && instance->range_unsignedInt <= 2, "range_unsignedInt", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_uint64(&instance->range_unsignedLong, 64, pstate);
    if (pstate->pu.error) return;
    validate_schema_range(instance->range_unsignedLong >= 1 && instance->range_unsignedLong <= 2, "range_unsignedLong", &pstate->pu);
    if (pstate->pu.error) return;
    parse_be_uint16(&instance->range_unsignedShort, 16, pstate);
    if (pstate->pu.error) return;
    validate_schema_range(instance->range_unsignedShort >= 1 && instance->range_unsignedShort <= 2, "range_unsignedShort", &pstate->pu);
    if (pstate->pu.error) return;
}

static void
simple__unparseSelf(const simple_ *instance, UState *ustate)
{
    unparse_be_bool(instance->simple_boolean, 32, 1, 0, ustate);
    if (ustate->pu.error) return;
    unparse_be_int8(instance->simple_byte, 8, ustate);
    if (ustate->pu.error) return;
    unparse_be_double(instance->simple_double, 64, ustate);
    if (ustate->pu.error) return;
    unparse_be_float(instance->simple_float, 32, ustate);
    if (ustate->pu.error) return;
    unparse_hexBinary(instance->simple_hexBinary, ustate);
    if (ustate->pu.error) return;
    unparse_be_uint16(instance->simple_hexBinaryPrefixed.lengthInBytes, 16, ustate);
    if (ustate->pu.error) return;
    unparse_hexBinary(instance->simple_hexBinaryPrefixed, ustate);
    if (ustate->pu.error) return;
    unparse_be_int32(instance->simple_int, 32, ustate);
    if (ustate->pu.error) return;
    unparse_be_int32(instance->simple_integer, 32, ustate);
    if (ustate->pu.error) return;
    unparse_be_int64(instance->simple_long, 64, ustate);
    if (ustate->pu.error) return;
    unparse_be_uint32(instance->simple_nonNegativeInteger, 32, ustate);
    if (ustate->pu.error) return;
    unparse_be_int16(instance->simple_short, 16, ustate);
    if (ustate->pu.error) return;
    unparse_be_uint8(instance->simple_unsignedByte, 8, ustate);
    if (ustate->pu.error) return;
    unparse_be_uint32(instance->simple_unsignedInt, 32, ustate);
    if (ustate->pu.error) return;
    unparse_be_uint64(instance->simple_unsignedLong, 64, ustate);
    if (ustate->pu.error) return;
    unparse_be_uint16(instance->simple_unsignedShort, 16, ustate);
    if (ustate->pu.error) return;
    unparse_be_int8(instance->enum_byte, 8, ustate);
    if (ustate->pu.error) return;
    int64_t enums_enum_byte[] = {1, 2};
    validate_integer_enumeration(instance->enum_byte, 2, enums_enum_byte, "enum_byte", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_double(instance->enum_double, 64, ustate);
    if (ustate->pu.error) return;
    double enums_enum_double[] = {1.0, 2.0};
    validate_floatpt_enumeration(instance->enum_double, 2, enums_enum_double, "enum_double", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_float(instance->enum_float, 32, ustate);
    if (ustate->pu.error) return;
    double enums_enum_float[] = {1.0, 2.0};
    validate_floatpt_enumeration(instance->enum_float, 2, enums_enum_float, "enum_float", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_hexBinary(instance->enum_hexBinary, ustate);
    if (ustate->pu.error) return;
    uint8_t arrays_enum_hexBinary[][4] = {{0x11, 0x22, 0x33, 0x44}, {0xAA, 0xBB, 0xCC, 0xDD}};
    HexBinary enums_enum_hexBinary[] = {{arrays_enum_hexBinary[0], 4, false}, {arrays_enum_hexBinary[1], 4, false}};
    validate_hexbinary_enumeration(&instance->enum_hexBinary, 2, enums_enum_hexBinary, "enum_hexBinary", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_uint16(instance->enum_hexBinaryPrefixed.lengthInBytes, 16, ustate);
    if (ustate->pu.error) return;
    unparse_hexBinary(instance->enum_hexBinaryPrefixed, ustate);
    if (ustate->pu.error) return;
    uint8_t arrays_enum_hexBinaryPrefixed[][4] = {{0x11, 0x22, 0x33, 0x44}, {0xAA, 0xBB, 0xCC, 0xDD}};
    HexBinary enums_enum_hexBinaryPrefixed[] = {{arrays_enum_hexBinaryPrefixed[0], 4, false}, {arrays_enum_hexBinaryPrefixed[1], 4, false}};
    validate_hexbinary_enumeration(&instance->enum_hexBinaryPrefixed, 2, enums_enum_hexBinaryPrefixed, "enum_hexBinaryPrefixed", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_int32(instance->enum_int, 32, ustate);
    if (ustate->pu.error) return;
    int64_t enums_enum_int[] = {1, 2};
    validate_integer_enumeration(instance->enum_int, 2, enums_enum_int, "enum_int", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_int32(instance->enum_integer, 32, ustate);
    if (ustate->pu.error) return;
    int64_t enums_enum_integer[] = {1, 2};
    validate_integer_enumeration(instance->enum_integer, 2, enums_enum_integer, "enum_integer", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_int64(instance->enum_long, 64, ustate);
    if (ustate->pu.error) return;
    int64_t enums_enum_long[] = {1, 2};
    validate_integer_enumeration(instance->enum_long, 2, enums_enum_long, "enum_long", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_uint32(instance->enum_nonNegativeInteger, 32, ustate);
    if (ustate->pu.error) return;
    int64_t enums_enum_nonNegativeInteger[] = {1, 2};
    validate_integer_enumeration(instance->enum_nonNegativeInteger, 2, enums_enum_nonNegativeInteger, "enum_nonNegativeInteger", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_int16(instance->enum_short, 16, ustate);
    if (ustate->pu.error) return;
    int64_t enums_enum_short[] = {1, 2};
    validate_integer_enumeration(instance->enum_short, 2, enums_enum_short, "enum_short", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_uint8(instance->enum_unsignedByte, 8, ustate);
    if (ustate->pu.error) return;
    int64_t enums_enum_unsignedByte[] = {1, 2};
    validate_integer_enumeration(instance->enum_unsignedByte, 2, enums_enum_unsignedByte, "enum_unsignedByte", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_uint32(instance->enum_unsignedInt, 32, ustate);
    if (ustate->pu.error) return;
    int64_t enums_enum_unsignedInt[] = {1, 2};
    validate_integer_enumeration(instance->enum_unsignedInt, 2, enums_enum_unsignedInt, "enum_unsignedInt", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_uint64(instance->enum_unsignedLong, 64, ustate);
    if (ustate->pu.error) return;
    int64_t enums_enum_unsignedLong[] = {1, 2};
    validate_integer_enumeration(instance->enum_unsignedLong, 2, enums_enum_unsignedLong, "enum_unsignedLong", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_uint16(instance->enum_unsignedShort, 16, ustate);
    if (ustate->pu.error) return;
    int64_t enums_enum_unsignedShort[] = {1, 2};
    validate_integer_enumeration(instance->enum_unsignedShort, 2, enums_enum_unsignedShort, "enum_unsignedShort", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_int8(instance->range_byte, 8, ustate);
    if (ustate->pu.error) return;
    validate_schema_range(instance->range_byte > 0 && instance->range_byte <= 100, "range_byte", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_double(instance->range_double, 64, ustate);
    if (ustate->pu.error) return;
    validate_schema_range(instance->range_double >= 1.0 && instance->range_double <= 2.0, "range_double", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_float(instance->range_float, 32, ustate);
    if (ustate->pu.error) return;
    validate_schema_range(instance->range_float > 0.0 && instance->range_float < 1.0, "range_float", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_int32(instance->range_int, 32, ustate);
    if (ustate->pu.error) return;
    validate_schema_range(instance->range_int >= 1 && instance->range_int < 2, "range_int", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_int32(instance->range_integer, 32, ustate);
    if (ustate->pu.error) return;
    validate_schema_range(instance->range_integer >= 1 && instance->range_integer <= 2, "range_integer", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_int64(instance->range_long, 64, ustate);
    if (ustate->pu.error) return;
    validate_schema_range(instance->range_long >= 1 && instance->range_long <= 2, "range_long", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_uint32(instance->range_nonNegativeInteger, 32, ustate);
    if (ustate->pu.error) return;
    validate_schema_range(instance->range_nonNegativeInteger >= 1 && instance->range_nonNegativeInteger <= 2, "range_nonNegativeInteger", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_int16(instance->range_short, 16, ustate);
    if (ustate->pu.error) return;
    validate_schema_range(instance->range_short >= 1 && instance->range_short <= 2, "range_short", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_uint8(instance->range_unsignedByte, 8, ustate);
    if (ustate->pu.error) return;
    validate_schema_range(instance->range_unsignedByte >= 1 && instance->range_unsignedByte <= 2, "range_unsignedByte", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_uint32(instance->range_unsignedInt, 32, ustate);
    if (ustate->pu.error) return;
    validate_schema_range(instance->range_unsignedInt >= 1 && instance->range_unsignedInt <= 2, "range_unsignedInt", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_uint64(instance->range_unsignedLong, 64, ustate);
    if (ustate->pu.error) return;
    validate_schema_range(instance->range_unsignedLong >= 1 && instance->range_unsignedLong <= 2, "range_unsignedLong", &ustate->pu);
    if (ustate->pu.error) return;
    unparse_be_uint16(instance->range_unsignedShort, 16, ustate);
    if (ustate->pu.error) return;
    validate_schema_range(instance->range_unsignedShort >= 1 && instance->range_unsignedShort <= 2, "range_unsignedShort", &ustate->pu);
    if (ustate->pu.error) return;
}

// Get an infoset (optionally clearing it first) for parsing/walking

InfosetBase *
get_infoset(bool clear_infoset)
{
    static simple_ infoset;

    if (clear_infoset)
    {
        // If your infoset contains hexBinary prefixed length elements,
        // you may want to walk infoset first to free their malloc'ed
        // storage - we are not handling that case for now...
        memset(&infoset, 0, sizeof(infoset));
        simple__initERD(&infoset, (InfosetBase *)&infoset);
    }

    return &infoset._base;
}
