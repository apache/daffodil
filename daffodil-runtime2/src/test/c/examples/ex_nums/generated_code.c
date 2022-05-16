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

static void array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate);
static void array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate);
static void bigEndian_ex_nums__parseSelf(bigEndian_ex_nums_ *instance, PState *pstate);
static void bigEndian_ex_nums__unparseSelf(const bigEndian_ex_nums_ *instance, UState *ustate);
static void littleEndian_ex_nums__parseSelf(littleEndian_ex_nums_ *instance, PState *pstate);
static void littleEndian_ex_nums__unparseSelf(const littleEndian_ex_nums_ *instance, UState *ustate);
static void fixed_ex_nums__parseSelf(fixed_ex_nums_ *instance, PState *pstate);
static void fixed_ex_nums__unparseSelf(const fixed_ex_nums_ *instance, UState *ustate);
static void ex_nums__parseSelf(ex_nums_ *instance, PState *pstate);
static void ex_nums__unparseSelf(const ex_nums_ *instance, UState *ustate);

// Define metadata for the infoset

static const ERD be_boolean_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_boolean", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_float_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_float", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_int16_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD hexBinary2_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinary2", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD hexBinaryPrefixed_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinaryPrefixed", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const array_ex_nums_ array_ex_nums__compute_offsets;

static const size_t array_ex_nums__offsets[14] = {
    (const char *)&array_ex_nums__compute_offsets.be_boolean[0] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.be_boolean[1] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.be_float[0] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.be_float[1] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.be_float[2] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.be_int16[0] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.be_int16[1] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.be_int16[2] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.hexBinary2[0] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.hexBinary2[1] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.hexBinary2[2] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.hexBinaryPrefixed[0] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.hexBinaryPrefixed[1] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.hexBinaryPrefixed[2] - (const char *)&array_ex_nums__compute_offsets
};

static const ERD *array_ex_nums__childrenERDs[14] = {
    &be_boolean_array_ex_nums_ERD,
    &be_boolean_array_ex_nums_ERD,
    &be_float_array_ex_nums_ERD,
    &be_float_array_ex_nums_ERD,
    &be_float_array_ex_nums_ERD,
    &be_int16_array_ex_nums_ERD,
    &be_int16_array_ex_nums_ERD,
    &be_int16_array_ex_nums_ERD,
    &hexBinary2_array_ex_nums_ERD,
    &hexBinary2_array_ex_nums_ERD,
    &hexBinary2_array_ex_nums_ERD,
    &hexBinaryPrefixed_array_ex_nums_ERD,
    &hexBinaryPrefixed_array_ex_nums_ERD,
    &hexBinaryPrefixed_array_ex_nums_ERD
};

static const ERD array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "array", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX, // typeCode
    14, // numChildren
    array_ex_nums__offsets, // offsets
    array_ex_nums__childrenERDs, // childrenERDs
    (ERDParseSelf)&array_ex_nums__parseSelf, // parseSelf
    (ERDUnparseSelf)&array_ex_nums__unparseSelf, // unparseSelf
    NULL // initChoice
};

static const ERD be_bool16_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_bool16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_boolean_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_boolean", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_double_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_double", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_float_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_float", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_int16_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_int32_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_int64_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_int8_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT8, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_integer17_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_integer17", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_uint16_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_uint32_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_uint64_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT64, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_uint8_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT8, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD be_nonNegativeInteger31_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_nonNegativeInteger31", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD hexBinary4_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinary4", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD hexBinaryPrefixed_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinaryPrefixed", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const bigEndian_ex_nums_ bigEndian_ex_nums__compute_offsets;

static const size_t bigEndian_ex_nums__offsets[16] = {
    (const char *)&bigEndian_ex_nums__compute_offsets.be_bool16 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_boolean - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_double - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_float - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_int16 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_int32 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_int64 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_int8 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_integer17 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_uint16 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_uint32 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_uint64 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_uint8 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_nonNegativeInteger31 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.hexBinary4 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.hexBinaryPrefixed - (const char *)&bigEndian_ex_nums__compute_offsets
};

static const ERD *bigEndian_ex_nums__childrenERDs[16] = {
    &be_bool16_bigEndian_ex_nums_ERD,
    &be_boolean_bigEndian_ex_nums_ERD,
    &be_double_bigEndian_ex_nums_ERD,
    &be_float_bigEndian_ex_nums_ERD,
    &be_int16_bigEndian_ex_nums_ERD,
    &be_int32_bigEndian_ex_nums_ERD,
    &be_int64_bigEndian_ex_nums_ERD,
    &be_int8_bigEndian_ex_nums_ERD,
    &be_integer17_bigEndian_ex_nums_ERD,
    &be_uint16_bigEndian_ex_nums_ERD,
    &be_uint32_bigEndian_ex_nums_ERD,
    &be_uint64_bigEndian_ex_nums_ERD,
    &be_uint8_bigEndian_ex_nums_ERD,
    &be_nonNegativeInteger31_bigEndian_ex_nums_ERD,
    &hexBinary4_bigEndian_ex_nums_ERD,
    &hexBinaryPrefixed_bigEndian_ex_nums_ERD
};

static const ERD bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "bigEndian", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX, // typeCode
    16, // numChildren
    bigEndian_ex_nums__offsets, // offsets
    bigEndian_ex_nums__childrenERDs, // childrenERDs
    (ERDParseSelf)&bigEndian_ex_nums__parseSelf, // parseSelf
    (ERDUnparseSelf)&bigEndian_ex_nums__unparseSelf, // unparseSelf
    NULL // initChoice
};

static const ERD le_bool16_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_bool16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_boolean_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_boolean", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_double_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_double", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_float_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_float", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_int16_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_int32_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_int64_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_int8_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT8, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_integer46_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_integer46", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_uint16_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_uint32_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_uint64_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT64, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_uint8_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT8, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD le_nonNegativeInteger10_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_nonNegativeInteger10", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD hexBinary0_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinary0", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD hexBinaryPrefixed_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinaryPrefixed", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const littleEndian_ex_nums_ littleEndian_ex_nums__compute_offsets;

static const size_t littleEndian_ex_nums__offsets[16] = {
    (const char *)&littleEndian_ex_nums__compute_offsets.le_bool16 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_boolean - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_double - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_float - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_int16 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_int32 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_int64 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_int8 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_integer46 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_uint16 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_uint32 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_uint64 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_uint8 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_nonNegativeInteger10 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.hexBinary0 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.hexBinaryPrefixed - (const char *)&littleEndian_ex_nums__compute_offsets
};

static const ERD *littleEndian_ex_nums__childrenERDs[16] = {
    &le_bool16_littleEndian_ex_nums_ERD,
    &le_boolean_littleEndian_ex_nums_ERD,
    &le_double_littleEndian_ex_nums_ERD,
    &le_float_littleEndian_ex_nums_ERD,
    &le_int16_littleEndian_ex_nums_ERD,
    &le_int32_littleEndian_ex_nums_ERD,
    &le_int64_littleEndian_ex_nums_ERD,
    &le_int8_littleEndian_ex_nums_ERD,
    &le_integer46_littleEndian_ex_nums_ERD,
    &le_uint16_littleEndian_ex_nums_ERD,
    &le_uint32_littleEndian_ex_nums_ERD,
    &le_uint64_littleEndian_ex_nums_ERD,
    &le_uint8_littleEndian_ex_nums_ERD,
    &le_nonNegativeInteger10_littleEndian_ex_nums_ERD,
    &hexBinary0_littleEndian_ex_nums_ERD,
    &hexBinaryPrefixed_littleEndian_ex_nums_ERD
};

static const ERD littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "littleEndian", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX, // typeCode
    16, // numChildren
    littleEndian_ex_nums__offsets, // offsets
    littleEndian_ex_nums__childrenERDs, // childrenERDs
    (ERDParseSelf)&littleEndian_ex_nums__parseSelf, // parseSelf
    (ERDUnparseSelf)&littleEndian_ex_nums__unparseSelf, // unparseSelf
    NULL // initChoice
};

static const ERD boolean_false_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "boolean_false", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD boolean_true_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "boolean_true", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD double_3_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "double_3", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD float_1_5_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "float_1_5", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD int_32_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "int_32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD hexBinary_deadbeef_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinary_deadbeef", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD hexBinary0_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinary0", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const ERD hexBinaryPrefixed_ab_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinaryPrefixed_ab", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, NULL
};

static const fixed_ex_nums_ fixed_ex_nums__compute_offsets;

static const size_t fixed_ex_nums__offsets[8] = {
    (const char *)&fixed_ex_nums__compute_offsets.boolean_false - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.boolean_true - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.double_3 - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.float_1_5 - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.int_32 - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.hexBinary_deadbeef - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.hexBinary0 - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.hexBinaryPrefixed_ab - (const char *)&fixed_ex_nums__compute_offsets
};

static const ERD *fixed_ex_nums__childrenERDs[8] = {
    &boolean_false_fixed_ex_nums_ERD,
    &boolean_true_fixed_ex_nums_ERD,
    &double_3_fixed_ex_nums_ERD,
    &float_1_5_fixed_ex_nums_ERD,
    &int_32_fixed_ex_nums_ERD,
    &hexBinary_deadbeef_fixed_ex_nums_ERD,
    &hexBinary0_fixed_ex_nums_ERD,
    &hexBinaryPrefixed_ab_fixed_ex_nums_ERD
};

static const ERD fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "fixed", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX, // typeCode
    8, // numChildren
    fixed_ex_nums__offsets, // offsets
    fixed_ex_nums__childrenERDs, // childrenERDs
    (ERDParseSelf)&fixed_ex_nums__parseSelf, // parseSelf
    (ERDUnparseSelf)&fixed_ex_nums__unparseSelf, // unparseSelf
    NULL // initChoice
};

static const ex_nums_ ex_nums__compute_offsets;

static const size_t ex_nums__offsets[4] = {
    (const char *)&ex_nums__compute_offsets.array - (const char *)&ex_nums__compute_offsets,
    (const char *)&ex_nums__compute_offsets.bigEndian - (const char *)&ex_nums__compute_offsets,
    (const char *)&ex_nums__compute_offsets.littleEndian - (const char *)&ex_nums__compute_offsets,
    (const char *)&ex_nums__compute_offsets.fixed - (const char *)&ex_nums__compute_offsets
};

static const ERD *ex_nums__childrenERDs[4] = {
    &array_ex_nums_ERD,
    &bigEndian_ex_nums_ERD,
    &littleEndian_ex_nums_ERD,
    &fixed_ex_nums_ERD
};

static const ERD ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "ex_nums", // namedQName.local
        "http://example.com", // namedQName.ns
    },
    COMPLEX, // typeCode
    4, // numChildren
    ex_nums__offsets, // offsets
    ex_nums__childrenERDs, // childrenERDs
    (ERDParseSelf)&ex_nums__parseSelf, // parseSelf
    (ERDUnparseSelf)&ex_nums__unparseSelf, // unparseSelf
    NULL // initChoice
};

// Initialize, parse, and unparse nodes of the infoset

static void
array_ex_nums__initERD(array_ex_nums_ *instance)
{
    instance->_base.erd = &array_ex_nums_ERD;
    instance->hexBinary2[0].array = instance->_a_hexBinary2[0];
    instance->hexBinary2[0].lengthInBytes = sizeof(instance->_a_hexBinary2[0]);
    instance->hexBinary2[0].dynamic = false;
    instance->hexBinary2[1].array = instance->_a_hexBinary2[1];
    instance->hexBinary2[1].lengthInBytes = sizeof(instance->_a_hexBinary2[1]);
    instance->hexBinary2[1].dynamic = false;
    instance->hexBinary2[2].array = instance->_a_hexBinary2[2];
    instance->hexBinary2[2].lengthInBytes = sizeof(instance->_a_hexBinary2[2]);
    instance->hexBinary2[2].dynamic = false;
    instance->hexBinaryPrefixed[0].array = NULL;
    instance->hexBinaryPrefixed[0].lengthInBytes = 0;
    instance->hexBinaryPrefixed[0].dynamic = true;
    instance->hexBinaryPrefixed[1].array = NULL;
    instance->hexBinaryPrefixed[1].lengthInBytes = 0;
    instance->hexBinaryPrefixed[1].dynamic = true;
    instance->hexBinaryPrefixed[2].array = NULL;
    instance->hexBinaryPrefixed[2].lengthInBytes = 0;
    instance->hexBinaryPrefixed[2].dynamic = true;
}

static void
array_ex_nums__initSelf(array_ex_nums_ *instance)
{
    instance->be_boolean[0] = true;
    instance->be_boolean[1] = true;
    instance->be_float[0] = NAN;
    instance->be_float[1] = NAN;
    instance->be_float[2] = NAN;
    instance->be_int16[0] = 0x7777;
    instance->be_int16[1] = 0x7777;
    instance->be_int16[2] = 0x7777;
    memset(instance->_a_hexBinary2[0], 0x77, sizeof(instance->_a_hexBinary2[0]));
    memset(instance->_a_hexBinary2[1], 0x77, sizeof(instance->_a_hexBinary2[1]));
    memset(instance->_a_hexBinary2[2], 0x77, sizeof(instance->_a_hexBinary2[2]));
}

static void
array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate)
{
    parse_be_bool(&instance->be_boolean[0], 32, -1, 0, pstate);
    if (pstate->error) return;
    parse_be_bool(&instance->be_boolean[1], 32, -1, 0, pstate);
    if (pstate->error) return;
    parse_be_float(&instance->be_float[0], 32, pstate);
    if (pstate->error) return;
    parse_be_float(&instance->be_float[1], 32, pstate);
    if (pstate->error) return;
    parse_be_float(&instance->be_float[2], 32, pstate);
    if (pstate->error) return;
    parse_be_int16(&instance->be_int16[0], 16, pstate);
    if (pstate->error) return;
    parse_be_int16(&instance->be_int16[1], 16, pstate);
    if (pstate->error) return;
    parse_be_int16(&instance->be_int16[2], 16, pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinary2[0], pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinary2[1], pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinary2[2], pstate);
    if (pstate->error) return;
    uint16_t _l_hexBinaryPrefixed0;
    parse_be_uint16(&_l_hexBinaryPrefixed0, 16, pstate);
    if (pstate->error) return;
    alloc_hexBinary(&instance->hexBinaryPrefixed[0], _l_hexBinaryPrefixed0, pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinaryPrefixed[0], pstate);
    if (pstate->error) return;
    uint16_t _l_hexBinaryPrefixed1;
    parse_be_uint16(&_l_hexBinaryPrefixed1, 16, pstate);
    if (pstate->error) return;
    alloc_hexBinary(&instance->hexBinaryPrefixed[1], _l_hexBinaryPrefixed1, pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinaryPrefixed[1], pstate);
    if (pstate->error) return;
    uint16_t _l_hexBinaryPrefixed2;
    parse_be_uint16(&_l_hexBinaryPrefixed2, 16, pstate);
    if (pstate->error) return;
    alloc_hexBinary(&instance->hexBinaryPrefixed[2], _l_hexBinaryPrefixed2, pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinaryPrefixed[2], pstate);
    if (pstate->error) return;
}

static void
array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate)
{
    unparse_be_bool(instance->be_boolean[0], 32, ~0, 0, ustate);
    if (ustate->error) return;
    unparse_be_bool(instance->be_boolean[1], 32, ~0, 0, ustate);
    if (ustate->error) return;
    unparse_be_float(instance->be_float[0], 32, ustate);
    if (ustate->error) return;
    unparse_be_float(instance->be_float[1], 32, ustate);
    if (ustate->error) return;
    unparse_be_float(instance->be_float[2], 32, ustate);
    if (ustate->error) return;
    unparse_be_int16(instance->be_int16[0], 16, ustate);
    if (ustate->error) return;
    unparse_be_int16(instance->be_int16[1], 16, ustate);
    if (ustate->error) return;
    unparse_be_int16(instance->be_int16[2], 16, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinary2[0], ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinary2[1], ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinary2[2], ustate);
    if (ustate->error) return;
    unparse_be_uint16(instance->hexBinaryPrefixed[0].lengthInBytes, 16, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinaryPrefixed[0], ustate);
    if (ustate->error) return;
    unparse_be_uint16(instance->hexBinaryPrefixed[1].lengthInBytes, 16, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinaryPrefixed[1], ustate);
    if (ustate->error) return;
    unparse_be_uint16(instance->hexBinaryPrefixed[2].lengthInBytes, 16, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinaryPrefixed[2], ustate);
    if (ustate->error) return;
}

static void
bigEndian_ex_nums__initERD(bigEndian_ex_nums_ *instance)
{
    instance->_base.erd = &bigEndian_ex_nums_ERD;
    instance->hexBinary4.array = instance->_a_hexBinary4;
    instance->hexBinary4.lengthInBytes = sizeof(instance->_a_hexBinary4);
    instance->hexBinary4.dynamic = false;
    instance->hexBinaryPrefixed.array = NULL;
    instance->hexBinaryPrefixed.lengthInBytes = 0;
    instance->hexBinaryPrefixed.dynamic = true;
}

static void
bigEndian_ex_nums__initSelf(bigEndian_ex_nums_ *instance)
{
    instance->be_bool16 = true;
    instance->be_boolean = true;
    instance->be_double = NAN;
    instance->be_float = NAN;
    instance->be_int16 = 0x7777;
    instance->be_int32 = 0x77777777;
    instance->be_int64 = 0x7777777777777777;
    instance->be_int8 = 0x77;
    instance->be_integer17 = 0x77777777;
    instance->be_uint16 = 0x7777;
    instance->be_uint32 = 0x77777777;
    instance->be_uint64 = 0x7777777777777777;
    instance->be_uint8 = 0x77;
    instance->be_nonNegativeInteger31 = 0x77777777;
    memset(instance->_a_hexBinary4, 0x77, sizeof(instance->_a_hexBinary4));
}

static void
bigEndian_ex_nums__parseSelf(bigEndian_ex_nums_ *instance, PState *pstate)
{
    parse_be_bool(&instance->be_bool16, 16, 16, 0, pstate);
    if (pstate->error) return;
    parse_be_bool(&instance->be_boolean, 32, -1, 0, pstate);
    if (pstate->error) return;
    parse_be_double(&instance->be_double, 64, pstate);
    if (pstate->error) return;
    parse_be_float(&instance->be_float, 32, pstate);
    if (pstate->error) return;
    parse_be_int16(&instance->be_int16, 16, pstate);
    if (pstate->error) return;
    parse_be_int32(&instance->be_int32, 32, pstate);
    if (pstate->error) return;
    parse_be_int64(&instance->be_int64, 64, pstate);
    if (pstate->error) return;
    parse_be_int8(&instance->be_int8, 8, pstate);
    if (pstate->error) return;
    parse_be_int32(&instance->be_integer17, 17, pstate);
    if (pstate->error) return;
    parse_be_uint16(&instance->be_uint16, 16, pstate);
    if (pstate->error) return;
    parse_be_uint32(&instance->be_uint32, 32, pstate);
    if (pstate->error) return;
    parse_be_uint64(&instance->be_uint64, 64, pstate);
    if (pstate->error) return;
    parse_be_uint8(&instance->be_uint8, 8, pstate);
    if (pstate->error) return;
    parse_be_uint32(&instance->be_nonNegativeInteger31, 31, pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinary4, pstate);
    if (pstate->error) return;
    uint16_t _l_hexBinaryPrefixed;
    parse_be_uint16(&_l_hexBinaryPrefixed, 16, pstate);
    if (pstate->error) return;
    alloc_hexBinary(&instance->hexBinaryPrefixed, _l_hexBinaryPrefixed, pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinaryPrefixed, pstate);
    if (pstate->error) return;
}

static void
bigEndian_ex_nums__unparseSelf(const bigEndian_ex_nums_ *instance, UState *ustate)
{
    unparse_be_bool(instance->be_bool16, 16, 16, 0, ustate);
    if (ustate->error) return;
    unparse_be_bool(instance->be_boolean, 32, ~0, 0, ustate);
    if (ustate->error) return;
    unparse_be_double(instance->be_double, 64, ustate);
    if (ustate->error) return;
    unparse_be_float(instance->be_float, 32, ustate);
    if (ustate->error) return;
    unparse_be_int16(instance->be_int16, 16, ustate);
    if (ustate->error) return;
    unparse_be_int32(instance->be_int32, 32, ustate);
    if (ustate->error) return;
    unparse_be_int64(instance->be_int64, 64, ustate);
    if (ustate->error) return;
    unparse_be_int8(instance->be_int8, 8, ustate);
    if (ustate->error) return;
    unparse_be_int32(instance->be_integer17, 17, ustate);
    if (ustate->error) return;
    unparse_be_uint16(instance->be_uint16, 16, ustate);
    if (ustate->error) return;
    unparse_be_uint32(instance->be_uint32, 32, ustate);
    if (ustate->error) return;
    unparse_be_uint64(instance->be_uint64, 64, ustate);
    if (ustate->error) return;
    unparse_be_uint8(instance->be_uint8, 8, ustate);
    if (ustate->error) return;
    unparse_be_uint32(instance->be_nonNegativeInteger31, 31, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinary4, ustate);
    if (ustate->error) return;
    unparse_be_uint16(instance->hexBinaryPrefixed.lengthInBytes, 16, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinaryPrefixed, ustate);
    if (ustate->error) return;
}

static void
littleEndian_ex_nums__initERD(littleEndian_ex_nums_ *instance)
{
    instance->_base.erd = &littleEndian_ex_nums_ERD;
    instance->hexBinary0.array = NULL;
    instance->hexBinary0.lengthInBytes = 0;
    instance->hexBinary0.dynamic = false;
    instance->hexBinaryPrefixed.array = NULL;
    instance->hexBinaryPrefixed.lengthInBytes = 0;
    instance->hexBinaryPrefixed.dynamic = true;
}

static void
littleEndian_ex_nums__initSelf(littleEndian_ex_nums_ *instance)
{
    instance->le_bool16 = true;
    instance->le_boolean = true;
    instance->le_double = NAN;
    instance->le_float = NAN;
    instance->le_int16 = 0x7777;
    instance->le_int32 = 0x77777777;
    instance->le_int64 = 0x7777777777777777;
    instance->le_int8 = 0x77;
    instance->le_integer46 = 0x7777777777777777;
    instance->le_uint16 = 0x7777;
    instance->le_uint32 = 0x77777777;
    instance->le_uint64 = 0x7777777777777777;
    instance->le_uint8 = 0x77;
    instance->le_nonNegativeInteger10 = 0x7777;
}

static void
littleEndian_ex_nums__parseSelf(littleEndian_ex_nums_ *instance, PState *pstate)
{
    parse_le_bool(&instance->le_bool16, 16, 16, 0, pstate);
    if (pstate->error) return;
    parse_le_bool(&instance->le_boolean, 32, -1, 0, pstate);
    if (pstate->error) return;
    parse_le_double(&instance->le_double, 64, pstate);
    if (pstate->error) return;
    parse_le_float(&instance->le_float, 32, pstate);
    if (pstate->error) return;
    parse_le_int16(&instance->le_int16, 16, pstate);
    if (pstate->error) return;
    parse_le_int32(&instance->le_int32, 32, pstate);
    if (pstate->error) return;
    parse_le_int64(&instance->le_int64, 64, pstate);
    if (pstate->error) return;
    parse_le_int8(&instance->le_int8, 8, pstate);
    if (pstate->error) return;
    parse_le_int64(&instance->le_integer46, 46, pstate);
    if (pstate->error) return;
    parse_le_uint16(&instance->le_uint16, 16, pstate);
    if (pstate->error) return;
    parse_le_uint32(&instance->le_uint32, 32, pstate);
    if (pstate->error) return;
    parse_le_uint64(&instance->le_uint64, 64, pstate);
    if (pstate->error) return;
    parse_le_uint8(&instance->le_uint8, 8, pstate);
    if (pstate->error) return;
    parse_le_uint16(&instance->le_nonNegativeInteger10, 10, pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinary0, pstate);
    if (pstate->error) return;
    uint16_t _l_hexBinaryPrefixed;
    parse_le_uint16(&_l_hexBinaryPrefixed, 16, pstate);
    if (pstate->error) return;
    alloc_hexBinary(&instance->hexBinaryPrefixed, _l_hexBinaryPrefixed, pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinaryPrefixed, pstate);
    if (pstate->error) return;
}

static void
littleEndian_ex_nums__unparseSelf(const littleEndian_ex_nums_ *instance, UState *ustate)
{
    unparse_le_bool(instance->le_bool16, 16, 16, 0, ustate);
    if (ustate->error) return;
    unparse_le_bool(instance->le_boolean, 32, ~0, 0, ustate);
    if (ustate->error) return;
    unparse_le_double(instance->le_double, 64, ustate);
    if (ustate->error) return;
    unparse_le_float(instance->le_float, 32, ustate);
    if (ustate->error) return;
    unparse_le_int16(instance->le_int16, 16, ustate);
    if (ustate->error) return;
    unparse_le_int32(instance->le_int32, 32, ustate);
    if (ustate->error) return;
    unparse_le_int64(instance->le_int64, 64, ustate);
    if (ustate->error) return;
    unparse_le_int8(instance->le_int8, 8, ustate);
    if (ustate->error) return;
    unparse_le_int64(instance->le_integer46, 46, ustate);
    if (ustate->error) return;
    unparse_le_uint16(instance->le_uint16, 16, ustate);
    if (ustate->error) return;
    unparse_le_uint32(instance->le_uint32, 32, ustate);
    if (ustate->error) return;
    unparse_le_uint64(instance->le_uint64, 64, ustate);
    if (ustate->error) return;
    unparse_le_uint8(instance->le_uint8, 8, ustate);
    if (ustate->error) return;
    unparse_le_uint16(instance->le_nonNegativeInteger10, 10, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinary0, ustate);
    if (ustate->error) return;
    unparse_le_uint16(instance->hexBinaryPrefixed.lengthInBytes, 16, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinaryPrefixed, ustate);
    if (ustate->error) return;
}

static void
fixed_ex_nums__initERD(fixed_ex_nums_ *instance)
{
    instance->_base.erd = &fixed_ex_nums_ERD;
    instance->hexBinary_deadbeef.array = instance->_a_hexBinary_deadbeef;
    instance->hexBinary_deadbeef.lengthInBytes = sizeof(instance->_a_hexBinary_deadbeef);
    instance->hexBinary_deadbeef.dynamic = false;
    instance->hexBinary0.array = NULL;
    instance->hexBinary0.lengthInBytes = 0;
    instance->hexBinary0.dynamic = false;
    instance->hexBinaryPrefixed_ab.array = NULL;
    instance->hexBinaryPrefixed_ab.lengthInBytes = 0;
    instance->hexBinaryPrefixed_ab.dynamic = true;
}

static void
fixed_ex_nums__initSelf(fixed_ex_nums_ *instance)
{
    instance->boolean_false = true;
    instance->boolean_true = true;
    instance->double_3 = NAN;
    instance->float_1_5 = NAN;
    instance->int_32 = 0x77777777;
    memset(instance->_a_hexBinary_deadbeef, 0x77, sizeof(instance->_a_hexBinary_deadbeef));
}

static void
fixed_ex_nums__parseSelf(fixed_ex_nums_ *instance, PState *pstate)
{
    parse_be_bool(&instance->boolean_false, 32, -1, 0, pstate);
    if (pstate->error) return;
    parse_validate_fixed(instance->boolean_false == false, "boolean_false", pstate);
    if (pstate->error) return;
    parse_be_bool(&instance->boolean_true, 32, -1, 0, pstate);
    if (pstate->error) return;
    parse_validate_fixed(instance->boolean_true == true, "boolean_true", pstate);
    if (pstate->error) return;
    parse_be_double(&instance->double_3, 64, pstate);
    if (pstate->error) return;
    parse_validate_fixed(instance->double_3 == 3.0, "double_3", pstate);
    if (pstate->error) return;
    parse_be_float(&instance->float_1_5, 32, pstate);
    if (pstate->error) return;
    parse_validate_fixed(instance->float_1_5 == 1.5, "float_1_5", pstate);
    if (pstate->error) return;
    parse_be_int32(&instance->int_32, 32, pstate);
    if (pstate->error) return;
    parse_validate_fixed(instance->int_32 == 32, "int_32", pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinary_deadbeef, pstate);
    if (pstate->error) return;
    uint8_t hexBinary_deadbeef_fixed[] = {0xDE, 0xAD, 0xBE, 0xEF};
    parse_validate_fixed(memcmp(instance->hexBinary_deadbeef.array, hexBinary_deadbeef_fixed, sizeof(hexBinary_deadbeef_fixed)) == 0, "hexBinary_deadbeef", pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinary0, pstate);
    if (pstate->error) return;
    int8_t _l_hexBinaryPrefixed_ab;
    parse_be_int8(&_l_hexBinaryPrefixed_ab, 8, pstate);
    if (pstate->error) return;
    alloc_hexBinary(&instance->hexBinaryPrefixed_ab, _l_hexBinaryPrefixed_ab, pstate);
    if (pstate->error) return;
    parse_hexBinary(&instance->hexBinaryPrefixed_ab, pstate);
    if (pstate->error) return;
    uint8_t hexBinaryPrefixed_ab_fixed[] = {0xAB};
    parse_validate_fixed(memcmp(instance->hexBinaryPrefixed_ab.array, hexBinaryPrefixed_ab_fixed, sizeof(hexBinaryPrefixed_ab_fixed)) == 0, "hexBinaryPrefixed_ab", pstate);
    if (pstate->error) return;
}

static void
fixed_ex_nums__unparseSelf(const fixed_ex_nums_ *instance, UState *ustate)
{
    unparse_be_bool(instance->boolean_false, 32, ~0, 0, ustate);
    if (ustate->error) return;
    unparse_validate_fixed(instance->boolean_false == false, "boolean_false", ustate);
    if (ustate->error) return;
    unparse_be_bool(instance->boolean_true, 32, ~0, 0, ustate);
    if (ustate->error) return;
    unparse_validate_fixed(instance->boolean_true == true, "boolean_true", ustate);
    if (ustate->error) return;
    unparse_be_double(instance->double_3, 64, ustate);
    if (ustate->error) return;
    unparse_validate_fixed(instance->double_3 == 3.0, "double_3", ustate);
    if (ustate->error) return;
    unparse_be_float(instance->float_1_5, 32, ustate);
    if (ustate->error) return;
    unparse_validate_fixed(instance->float_1_5 == 1.5, "float_1_5", ustate);
    if (ustate->error) return;
    unparse_be_int32(instance->int_32, 32, ustate);
    if (ustate->error) return;
    unparse_validate_fixed(instance->int_32 == 32, "int_32", ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinary_deadbeef, ustate);
    if (ustate->error) return;
    uint8_t hexBinary_deadbeef_fixed[] = {0xDE, 0xAD, 0xBE, 0xEF};
    unparse_validate_fixed(memcmp(instance->hexBinary_deadbeef.array, hexBinary_deadbeef_fixed, sizeof(hexBinary_deadbeef_fixed)) == 0, "hexBinary_deadbeef", ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinary0, ustate);
    if (ustate->error) return;
    unparse_be_int8(instance->hexBinaryPrefixed_ab.lengthInBytes, 8, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinaryPrefixed_ab, ustate);
    if (ustate->error) return;
    uint8_t hexBinaryPrefixed_ab_fixed[] = {0xAB};
    unparse_validate_fixed(memcmp(instance->hexBinaryPrefixed_ab.array, hexBinaryPrefixed_ab_fixed, sizeof(hexBinaryPrefixed_ab_fixed)) == 0, "hexBinaryPrefixed_ab", ustate);
    if (ustate->error) return;
}

static void
ex_nums__initERD(ex_nums_ *instance)
{
    instance->_base.erd = &ex_nums_ERD;
    array_ex_nums__initERD(&instance->array);
    bigEndian_ex_nums__initERD(&instance->bigEndian);
    littleEndian_ex_nums__initERD(&instance->littleEndian);
    fixed_ex_nums__initERD(&instance->fixed);
}

static void
ex_nums__initSelf(ex_nums_ *instance)
{
    array_ex_nums__initSelf(&instance->array);
    bigEndian_ex_nums__initSelf(&instance->bigEndian);
    littleEndian_ex_nums__initSelf(&instance->littleEndian);
    fixed_ex_nums__initSelf(&instance->fixed);
}

static void
ex_nums__parseSelf(ex_nums_ *instance, PState *pstate)
{
    array_ex_nums__parseSelf(&instance->array, pstate);
    if (pstate->error) return;
    bigEndian_ex_nums__parseSelf(&instance->bigEndian, pstate);
    if (pstate->error) return;
    littleEndian_ex_nums__parseSelf(&instance->littleEndian, pstate);
    if (pstate->error) return;
    fixed_ex_nums__parseSelf(&instance->fixed, pstate);
    if (pstate->error) return;
}

static void
ex_nums__unparseSelf(const ex_nums_ *instance, UState *ustate)
{
    array_ex_nums__unparseSelf(&instance->array, ustate);
    if (ustate->error) return;
    bigEndian_ex_nums__unparseSelf(&instance->bigEndian, ustate);
    if (ustate->error) return;
    littleEndian_ex_nums__unparseSelf(&instance->littleEndian, ustate);
    if (ustate->error) return;
    fixed_ex_nums__unparseSelf(&instance->fixed, ustate);
    if (ustate->error) return;
}

// Return a root element for parsing or unparsing the infoset

InfosetBase *
rootElement(void)
{
    static bool initialized;
    static ex_nums_ root;
    if (!initialized)
    {
        ex_nums__initERD(&root);
        ex_nums__initSelf(&root);
        initialized = true;
    }
    return &root._base;
}
