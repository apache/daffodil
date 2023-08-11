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

static void array_be_boolean_array_ex_nums_array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate);
static void array_be_boolean_array_ex_nums_array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate);
static size_t array_be_boolean_array_ex_nums_array_ex_nums__getArraySize(const array_ex_nums_ *instance);
static void array_be_float_array_ex_nums_array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate);
static void array_be_float_array_ex_nums_array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate);
static size_t array_be_float_array_ex_nums_array_ex_nums__getArraySize(const array_ex_nums_ *instance);
static void array_be_int16_array_ex_nums_array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate);
static void array_be_int16_array_ex_nums_array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate);
static size_t array_be_int16_array_ex_nums_array_ex_nums__getArraySize(const array_ex_nums_ *instance);
static void array_hexBinary2_array_ex_nums_array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate);
static void array_hexBinary2_array_ex_nums_array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate);
static size_t array_hexBinary2_array_ex_nums_array_ex_nums__getArraySize(const array_ex_nums_ *instance);
static void array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate);
static void array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate);
static size_t array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__getArraySize(const array_ex_nums_ *instance);
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
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const array_ex_nums_ array_be_boolean_array_ex_nums_array_ex_nums__compute_offsets;

static const size_t array_be_boolean_array_ex_nums_array_ex_nums__childrenOffsets[1] = {
    (const char *)&array_be_boolean_array_ex_nums_array_ex_nums__compute_offsets.be_boolean[1] - (const char *)&array_be_boolean_array_ex_nums_array_ex_nums__compute_offsets.be_boolean[0]
};

static const ERD *const array_be_boolean_array_ex_nums_array_ex_nums__childrenERDs[1] = {
    &be_boolean_array_ex_nums_ERD
};

static const ERD array_be_boolean_array_ex_nums_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_boolean", // namedQName.local
        NULL, // namedQName.ns
    },
    ARRAY, // typeCode
    2, // maxOccurs
    array_be_boolean_array_ex_nums_array_ex_nums__childrenOffsets,
    array_be_boolean_array_ex_nums_array_ex_nums__childrenERDs,
    (ERDParseSelf)&array_be_boolean_array_ex_nums_array_ex_nums__parseSelf,
    (ERDUnparseSelf)&array_be_boolean_array_ex_nums_array_ex_nums__unparseSelf,
    {.getArraySize = (GetArraySize)&array_be_boolean_array_ex_nums_array_ex_nums__getArraySize}
};

static const ERD be_float_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_float", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const array_ex_nums_ array_be_float_array_ex_nums_array_ex_nums__compute_offsets;

static const size_t array_be_float_array_ex_nums_array_ex_nums__childrenOffsets[1] = {
    (const char *)&array_be_float_array_ex_nums_array_ex_nums__compute_offsets.be_float[1] - (const char *)&array_be_float_array_ex_nums_array_ex_nums__compute_offsets.be_float[0]
};

static const ERD *const array_be_float_array_ex_nums_array_ex_nums__childrenERDs[1] = {
    &be_float_array_ex_nums_ERD
};

static const ERD array_be_float_array_ex_nums_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_float", // namedQName.local
        NULL, // namedQName.ns
    },
    ARRAY, // typeCode
    3, // maxOccurs
    array_be_float_array_ex_nums_array_ex_nums__childrenOffsets,
    array_be_float_array_ex_nums_array_ex_nums__childrenERDs,
    (ERDParseSelf)&array_be_float_array_ex_nums_array_ex_nums__parseSelf,
    (ERDUnparseSelf)&array_be_float_array_ex_nums_array_ex_nums__unparseSelf,
    {.getArraySize = (GetArraySize)&array_be_float_array_ex_nums_array_ex_nums__getArraySize}
};

static const ERD be_int16_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const array_ex_nums_ array_be_int16_array_ex_nums_array_ex_nums__compute_offsets;

static const size_t array_be_int16_array_ex_nums_array_ex_nums__childrenOffsets[1] = {
    (const char *)&array_be_int16_array_ex_nums_array_ex_nums__compute_offsets.be_int16[1] - (const char *)&array_be_int16_array_ex_nums_array_ex_nums__compute_offsets.be_int16[0]
};

static const ERD *const array_be_int16_array_ex_nums_array_ex_nums__childrenERDs[1] = {
    &be_int16_array_ex_nums_ERD
};

static const ERD array_be_int16_array_ex_nums_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int16", // namedQName.local
        NULL, // namedQName.ns
    },
    ARRAY, // typeCode
    3, // maxOccurs
    array_be_int16_array_ex_nums_array_ex_nums__childrenOffsets,
    array_be_int16_array_ex_nums_array_ex_nums__childrenERDs,
    (ERDParseSelf)&array_be_int16_array_ex_nums_array_ex_nums__parseSelf,
    (ERDUnparseSelf)&array_be_int16_array_ex_nums_array_ex_nums__unparseSelf,
    {.getArraySize = (GetArraySize)&array_be_int16_array_ex_nums_array_ex_nums__getArraySize}
};

static const ERD hexBinary2_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinary2", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const array_ex_nums_ array_hexBinary2_array_ex_nums_array_ex_nums__compute_offsets;

static const size_t array_hexBinary2_array_ex_nums_array_ex_nums__childrenOffsets[1] = {
    (const char *)&array_hexBinary2_array_ex_nums_array_ex_nums__compute_offsets.hexBinary2[1] - (const char *)&array_hexBinary2_array_ex_nums_array_ex_nums__compute_offsets.hexBinary2[0]
};

static const ERD *const array_hexBinary2_array_ex_nums_array_ex_nums__childrenERDs[1] = {
    &hexBinary2_array_ex_nums_ERD
};

static const ERD array_hexBinary2_array_ex_nums_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinary2", // namedQName.local
        NULL, // namedQName.ns
    },
    ARRAY, // typeCode
    3, // maxOccurs
    array_hexBinary2_array_ex_nums_array_ex_nums__childrenOffsets,
    array_hexBinary2_array_ex_nums_array_ex_nums__childrenERDs,
    (ERDParseSelf)&array_hexBinary2_array_ex_nums_array_ex_nums__parseSelf,
    (ERDUnparseSelf)&array_hexBinary2_array_ex_nums_array_ex_nums__unparseSelf,
    {.getArraySize = (GetArraySize)&array_hexBinary2_array_ex_nums_array_ex_nums__getArraySize}
};

static const ERD hexBinaryPrefixed_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinaryPrefixed", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const array_ex_nums_ array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__compute_offsets;

static const size_t array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__childrenOffsets[1] = {
    (const char *)&array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__compute_offsets.hexBinaryPrefixed[1] - (const char *)&array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__compute_offsets.hexBinaryPrefixed[0]
};

static const ERD *const array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__childrenERDs[1] = {
    &hexBinaryPrefixed_array_ex_nums_ERD
};

static const ERD array_hexBinaryPrefixed_array_ex_nums_array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinaryPrefixed", // namedQName.local
        NULL, // namedQName.ns
    },
    ARRAY, // typeCode
    3, // maxOccurs
    array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__childrenOffsets,
    array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__childrenERDs,
    (ERDParseSelf)&array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__parseSelf,
    (ERDUnparseSelf)&array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__unparseSelf,
    {.getArraySize = (GetArraySize)&array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__getArraySize}
};

static const array_ex_nums_ array_ex_nums__compute_offsets;

static const size_t array_ex_nums__childrenOffsets[5] = {
    (const char *)&array_ex_nums__compute_offsets.be_boolean[0] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.be_float[0] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.be_int16[0] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.hexBinary2[0] - (const char *)&array_ex_nums__compute_offsets,
    (const char *)&array_ex_nums__compute_offsets.hexBinaryPrefixed[0] - (const char *)&array_ex_nums__compute_offsets
};

static const ERD *const array_ex_nums__childrenERDs[5] = {
    &array_be_boolean_array_ex_nums_array_ex_nums_ERD,
    &array_be_float_array_ex_nums_array_ex_nums_ERD,
    &array_be_int16_array_ex_nums_array_ex_nums_ERD,
    &array_hexBinary2_array_ex_nums_array_ex_nums_ERD,
    &array_hexBinaryPrefixed_array_ex_nums_array_ex_nums_ERD
};

static const ERD array_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "array", // namedQName.local
        NULL, // namedQName.ns
    },
    COMPLEX, // typeCode
    5, // numChildren
    array_ex_nums__childrenOffsets,
    array_ex_nums__childrenERDs,
    (ERDParseSelf)&array_ex_nums__parseSelf,
    (ERDUnparseSelf)&array_ex_nums__unparseSelf,
    {.initChoice = NULL}
};

static const ERD be_bool16_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_bool16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_boolean_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_boolean", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_double_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_double", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_float_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_float", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_int16_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_int32_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_int64_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_int8_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT8, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_int17_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_int17", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_uint16_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_uint32_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_uint64_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT64, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_uint8_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT8, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD be_uint31_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "be_uint31", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD hexBinary4_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinary4", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD hexBinaryPrefixed_bigEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinaryPrefixed", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const bigEndian_ex_nums_ bigEndian_ex_nums__compute_offsets;

static const size_t bigEndian_ex_nums__childrenOffsets[16] = {
    (const char *)&bigEndian_ex_nums__compute_offsets.be_bool16 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_boolean - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_double - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_float - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_int16 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_int32 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_int64 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_int8 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_int17 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_uint16 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_uint32 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_uint64 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_uint8 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.be_uint31 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.hexBinary4 - (const char *)&bigEndian_ex_nums__compute_offsets,
    (const char *)&bigEndian_ex_nums__compute_offsets.hexBinaryPrefixed - (const char *)&bigEndian_ex_nums__compute_offsets
};

static const ERD *const bigEndian_ex_nums__childrenERDs[16] = {
    &be_bool16_bigEndian_ex_nums_ERD,
    &be_boolean_bigEndian_ex_nums_ERD,
    &be_double_bigEndian_ex_nums_ERD,
    &be_float_bigEndian_ex_nums_ERD,
    &be_int16_bigEndian_ex_nums_ERD,
    &be_int32_bigEndian_ex_nums_ERD,
    &be_int64_bigEndian_ex_nums_ERD,
    &be_int8_bigEndian_ex_nums_ERD,
    &be_int17_bigEndian_ex_nums_ERD,
    &be_uint16_bigEndian_ex_nums_ERD,
    &be_uint32_bigEndian_ex_nums_ERD,
    &be_uint64_bigEndian_ex_nums_ERD,
    &be_uint8_bigEndian_ex_nums_ERD,
    &be_uint31_bigEndian_ex_nums_ERD,
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
    bigEndian_ex_nums__childrenOffsets,
    bigEndian_ex_nums__childrenERDs,
    (ERDParseSelf)&bigEndian_ex_nums__parseSelf,
    (ERDUnparseSelf)&bigEndian_ex_nums__unparseSelf,
    {.initChoice = NULL}
};

static const ERD le_bool16_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_bool16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_boolean_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_boolean", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_double_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_double", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_float_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_float", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_int16_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT16, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_int32_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_int64_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_int8_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT8, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_int46_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_int46", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT64, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_uint16_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint16", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_uint32_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_uint64_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint64", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT64, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_uint8_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint8", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT8, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD le_uint10_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "le_uint10", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_UINT16, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD hexBinary0_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinary0", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD hexBinaryPrefixed_littleEndian_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinaryPrefixed", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const littleEndian_ex_nums_ littleEndian_ex_nums__compute_offsets;

static const size_t littleEndian_ex_nums__childrenOffsets[16] = {
    (const char *)&littleEndian_ex_nums__compute_offsets.le_bool16 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_boolean - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_double - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_float - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_int16 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_int32 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_int64 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_int8 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_int46 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_uint16 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_uint32 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_uint64 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_uint8 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.le_uint10 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.hexBinary0 - (const char *)&littleEndian_ex_nums__compute_offsets,
    (const char *)&littleEndian_ex_nums__compute_offsets.hexBinaryPrefixed - (const char *)&littleEndian_ex_nums__compute_offsets
};

static const ERD *const littleEndian_ex_nums__childrenERDs[16] = {
    &le_bool16_littleEndian_ex_nums_ERD,
    &le_boolean_littleEndian_ex_nums_ERD,
    &le_double_littleEndian_ex_nums_ERD,
    &le_float_littleEndian_ex_nums_ERD,
    &le_int16_littleEndian_ex_nums_ERD,
    &le_int32_littleEndian_ex_nums_ERD,
    &le_int64_littleEndian_ex_nums_ERD,
    &le_int8_littleEndian_ex_nums_ERD,
    &le_int46_littleEndian_ex_nums_ERD,
    &le_uint16_littleEndian_ex_nums_ERD,
    &le_uint32_littleEndian_ex_nums_ERD,
    &le_uint64_littleEndian_ex_nums_ERD,
    &le_uint8_littleEndian_ex_nums_ERD,
    &le_uint10_littleEndian_ex_nums_ERD,
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
    littleEndian_ex_nums__childrenOffsets,
    littleEndian_ex_nums__childrenERDs,
    (ERDParseSelf)&littleEndian_ex_nums__parseSelf,
    (ERDUnparseSelf)&littleEndian_ex_nums__unparseSelf,
    {.initChoice = NULL}
};

static const ERD boolean_false_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "boolean_false", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD boolean_true_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "boolean_true", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_BOOLEAN, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD double_3_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "double_3", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_DOUBLE, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD float_1_5_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "float_1_5", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_FLOAT, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD int_32_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "int_32", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_INT32, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD hexBinary_deadbeef_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinary_deadbeef", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD hexBinary0_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinary0", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const ERD hexBinaryPrefixed_ab_fixed_ex_nums_ERD = {
    {
        NULL, // namedQName.prefix
        "hexBinaryPrefixed_ab", // namedQName.local
        NULL, // namedQName.ns
    },
    PRIMITIVE_HEXBINARY, // typeCode
    0, NULL, NULL, NULL, NULL, {NULL}
};

static const fixed_ex_nums_ fixed_ex_nums__compute_offsets;

static const size_t fixed_ex_nums__childrenOffsets[8] = {
    (const char *)&fixed_ex_nums__compute_offsets.boolean_false - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.boolean_true - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.double_3 - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.float_1_5 - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.int_32 - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.hexBinary_deadbeef - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.hexBinary0 - (const char *)&fixed_ex_nums__compute_offsets,
    (const char *)&fixed_ex_nums__compute_offsets.hexBinaryPrefixed_ab - (const char *)&fixed_ex_nums__compute_offsets
};

static const ERD *const fixed_ex_nums__childrenERDs[8] = {
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
    fixed_ex_nums__childrenOffsets,
    fixed_ex_nums__childrenERDs,
    (ERDParseSelf)&fixed_ex_nums__parseSelf,
    (ERDUnparseSelf)&fixed_ex_nums__unparseSelf,
    {.initChoice = NULL}
};

static const ex_nums_ ex_nums__compute_offsets;

static const size_t ex_nums__childrenOffsets[4] = {
    (const char *)&ex_nums__compute_offsets.array - (const char *)&ex_nums__compute_offsets,
    (const char *)&ex_nums__compute_offsets.bigEndian - (const char *)&ex_nums__compute_offsets,
    (const char *)&ex_nums__compute_offsets.littleEndian - (const char *)&ex_nums__compute_offsets,
    (const char *)&ex_nums__compute_offsets.fixed - (const char *)&ex_nums__compute_offsets
};

static const ERD *const ex_nums__childrenERDs[4] = {
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
    ex_nums__childrenOffsets,
    ex_nums__childrenERDs,
    (ERDParseSelf)&ex_nums__parseSelf,
    (ERDUnparseSelf)&ex_nums__unparseSelf,
    {.initChoice = NULL}
};

// Initialize, parse, and unparse nodes of the infoset

static void
array_be_boolean_array_ex_nums_array_ex_nums__initERD(array_ex_nums_ *instance, InfosetBase *parent)
{
    UNUSED(instance);
    UNUSED(parent);
}

static void
array_be_boolean_array_ex_nums_array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate)
{
    const size_t arraySize = array_be_boolean_array_ex_nums_array_ex_nums__getArraySize(instance);
    parse_check_bounds("array_be_boolean_array_ex_nums_array_ex_nums_", arraySize, 2, 2, pstate);
    if (pstate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        parse_be_bool(&instance->be_boolean[i], 32, -1, 0, pstate);
        if (pstate->error) return;
    }
}

static void
array_be_boolean_array_ex_nums_array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate)
{
    const size_t arraySize = array_be_boolean_array_ex_nums_array_ex_nums__getArraySize(instance);
    unparse_check_bounds("array_be_boolean_array_ex_nums_array_ex_nums_", arraySize, 2, 2, ustate);
    if (ustate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        unparse_be_bool(instance->be_boolean[i], 32, ~0, 0, ustate);
        if (ustate->error) return;
    }
}

static size_t
array_be_boolean_array_ex_nums_array_ex_nums__getArraySize(const array_ex_nums_ *instance)
{
    UNUSED(instance);
    return 2;
}

static void
array_be_float_array_ex_nums_array_ex_nums__initERD(array_ex_nums_ *instance, InfosetBase *parent)
{
    UNUSED(instance);
    UNUSED(parent);
}

static void
array_be_float_array_ex_nums_array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate)
{
    const size_t arraySize = array_be_float_array_ex_nums_array_ex_nums__getArraySize(instance);
    parse_check_bounds("array_be_float_array_ex_nums_array_ex_nums_", arraySize, 3, 3, pstate);
    if (pstate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        parse_be_float(&instance->be_float[i], 32, pstate);
        if (pstate->error) return;
    }
}

static void
array_be_float_array_ex_nums_array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate)
{
    const size_t arraySize = array_be_float_array_ex_nums_array_ex_nums__getArraySize(instance);
    unparse_check_bounds("array_be_float_array_ex_nums_array_ex_nums_", arraySize, 3, 3, ustate);
    if (ustate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        unparse_be_float(instance->be_float[i], 32, ustate);
        if (ustate->error) return;
    }
}

static size_t
array_be_float_array_ex_nums_array_ex_nums__getArraySize(const array_ex_nums_ *instance)
{
    UNUSED(instance);
    return 3;
}

static void
array_be_int16_array_ex_nums_array_ex_nums__initERD(array_ex_nums_ *instance, InfosetBase *parent)
{
    UNUSED(instance);
    UNUSED(parent);
}

static void
array_be_int16_array_ex_nums_array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate)
{
    const size_t arraySize = array_be_int16_array_ex_nums_array_ex_nums__getArraySize(instance);
    parse_check_bounds("array_be_int16_array_ex_nums_array_ex_nums_", arraySize, 3, 3, pstate);
    if (pstate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        parse_be_int16(&instance->be_int16[i], 16, pstate);
        if (pstate->error) return;
    }
}

static void
array_be_int16_array_ex_nums_array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate)
{
    const size_t arraySize = array_be_int16_array_ex_nums_array_ex_nums__getArraySize(instance);
    unparse_check_bounds("array_be_int16_array_ex_nums_array_ex_nums_", arraySize, 3, 3, ustate);
    if (ustate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        unparse_be_int16(instance->be_int16[i], 16, ustate);
        if (ustate->error) return;
    }
}

static size_t
array_be_int16_array_ex_nums_array_ex_nums__getArraySize(const array_ex_nums_ *instance)
{
    UNUSED(instance);
    return 3;
}

static void
array_hexBinary2_array_ex_nums_array_ex_nums__initERD(array_ex_nums_ *instance, InfosetBase *parent)
{
    UNUSED(parent);
    for (size_t i = 0; i < 3; i++)
    {
        instance->hexBinary2[i].array = instance->_a_hexBinary2[i];
        instance->hexBinary2[i].lengthInBytes = sizeof(instance->_a_hexBinary2[i]);
        instance->hexBinary2[i].dynamic = false;
    }
}

static void
array_hexBinary2_array_ex_nums_array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate)
{
    const size_t arraySize = array_hexBinary2_array_ex_nums_array_ex_nums__getArraySize(instance);
    parse_check_bounds("array_hexBinary2_array_ex_nums_array_ex_nums_", arraySize, 3, 3, pstate);
    if (pstate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        parse_hexBinary(&instance->hexBinary2[i], pstate);
        if (pstate->error) return;
    }
}

static void
array_hexBinary2_array_ex_nums_array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate)
{
    const size_t arraySize = array_hexBinary2_array_ex_nums_array_ex_nums__getArraySize(instance);
    unparse_check_bounds("array_hexBinary2_array_ex_nums_array_ex_nums_", arraySize, 3, 3, ustate);
    if (ustate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        unparse_hexBinary(instance->hexBinary2[i], ustate);
        if (ustate->error) return;
    }
}

static size_t
array_hexBinary2_array_ex_nums_array_ex_nums__getArraySize(const array_ex_nums_ *instance)
{
    UNUSED(instance);
    return 3;
}

static void
array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__initERD(array_ex_nums_ *instance, InfosetBase *parent)
{
    UNUSED(parent);
    for (size_t i = 0; i < 3; i++)
    {
        instance->hexBinaryPrefixed[i].dynamic = true;
    }
}

static void
array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate)
{
    const size_t arraySize = array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__getArraySize(instance);
    parse_check_bounds("array_hexBinaryPrefixed_array_ex_nums_array_ex_nums_", arraySize, 3, 3, pstate);
    if (pstate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        uint16_t _l_hexBinaryPrefixed;
        parse_be_uint16(&_l_hexBinaryPrefixed, 16, pstate);
        if (pstate->error) return;
        alloc_hexBinary(&instance->hexBinaryPrefixed[i], _l_hexBinaryPrefixed, pstate);
        if (pstate->error) return;
        parse_hexBinary(&instance->hexBinaryPrefixed[i], pstate);
        if (pstate->error) return;
    }
}

static void
array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate)
{
    const size_t arraySize = array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__getArraySize(instance);
    unparse_check_bounds("array_hexBinaryPrefixed_array_ex_nums_array_ex_nums_", arraySize, 3, 3, ustate);
    if (ustate->error) return;

    for (size_t i = 0; i < arraySize; i++)
    {
        unparse_be_uint16(instance->hexBinaryPrefixed[i].lengthInBytes, 16, ustate);
        if (ustate->error) return;
        unparse_hexBinary(instance->hexBinaryPrefixed[i], ustate);
        if (ustate->error) return;
    }
}

static size_t
array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__getArraySize(const array_ex_nums_ *instance)
{
    UNUSED(instance);
    return 3;
}

static void
array_ex_nums__initERD(array_ex_nums_ *instance, InfosetBase *parent)
{
    instance->_base.erd = &array_ex_nums_ERD;
    instance->_base.parent = parent;
    array_be_boolean_array_ex_nums_array_ex_nums__initERD(instance, parent);
    array_be_float_array_ex_nums_array_ex_nums__initERD(instance, parent);
    array_be_int16_array_ex_nums_array_ex_nums__initERD(instance, parent);
    array_hexBinary2_array_ex_nums_array_ex_nums__initERD(instance, parent);
    array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__initERD(instance, parent);
}

static void
array_ex_nums__parseSelf(array_ex_nums_ *instance, PState *pstate)
{
    array_be_boolean_array_ex_nums_array_ex_nums__parseSelf(instance, pstate);
    if (pstate->error) return;
    array_be_float_array_ex_nums_array_ex_nums__parseSelf(instance, pstate);
    if (pstate->error) return;
    array_be_int16_array_ex_nums_array_ex_nums__parseSelf(instance, pstate);
    if (pstate->error) return;
    array_hexBinary2_array_ex_nums_array_ex_nums__parseSelf(instance, pstate);
    if (pstate->error) return;
    array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__parseSelf(instance, pstate);
    if (pstate->error) return;
}

static void
array_ex_nums__unparseSelf(const array_ex_nums_ *instance, UState *ustate)
{
    array_be_boolean_array_ex_nums_array_ex_nums__unparseSelf(instance, ustate);
    if (ustate->error) return;
    array_be_float_array_ex_nums_array_ex_nums__unparseSelf(instance, ustate);
    if (ustate->error) return;
    array_be_int16_array_ex_nums_array_ex_nums__unparseSelf(instance, ustate);
    if (ustate->error) return;
    array_hexBinary2_array_ex_nums_array_ex_nums__unparseSelf(instance, ustate);
    if (ustate->error) return;
    array_hexBinaryPrefixed_array_ex_nums_array_ex_nums__unparseSelf(instance, ustate);
    if (ustate->error) return;
}

static void
bigEndian_ex_nums__initERD(bigEndian_ex_nums_ *instance, InfosetBase *parent)
{
    instance->_base.erd = &bigEndian_ex_nums_ERD;
    instance->_base.parent = parent;
    instance->hexBinary4.array = instance->_a_hexBinary4;
    instance->hexBinary4.lengthInBytes = sizeof(instance->_a_hexBinary4);
    instance->hexBinary4.dynamic = false;
    instance->hexBinaryPrefixed.dynamic = true;
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
    parse_be_int32(&instance->be_int17, 17, pstate);
    if (pstate->error) return;
    parse_be_uint16(&instance->be_uint16, 16, pstate);
    if (pstate->error) return;
    parse_be_uint32(&instance->be_uint32, 32, pstate);
    if (pstate->error) return;
    parse_be_uint64(&instance->be_uint64, 64, pstate);
    if (pstate->error) return;
    parse_be_uint8(&instance->be_uint8, 8, pstate);
    if (pstate->error) return;
    parse_be_uint32(&instance->be_uint31, 31, pstate);
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
    unparse_be_int32(instance->be_int17, 17, ustate);
    if (ustate->error) return;
    unparse_be_uint16(instance->be_uint16, 16, ustate);
    if (ustate->error) return;
    unparse_be_uint32(instance->be_uint32, 32, ustate);
    if (ustate->error) return;
    unparse_be_uint64(instance->be_uint64, 64, ustate);
    if (ustate->error) return;
    unparse_be_uint8(instance->be_uint8, 8, ustate);
    if (ustate->error) return;
    unparse_be_uint32(instance->be_uint31, 31, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinary4, ustate);
    if (ustate->error) return;
    unparse_be_uint16(instance->hexBinaryPrefixed.lengthInBytes, 16, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinaryPrefixed, ustate);
    if (ustate->error) return;
}

static void
littleEndian_ex_nums__initERD(littleEndian_ex_nums_ *instance, InfosetBase *parent)
{
    instance->_base.erd = &littleEndian_ex_nums_ERD;
    instance->_base.parent = parent;
    instance->hexBinary0.array = NULL;
    instance->hexBinary0.lengthInBytes = 0;
    instance->hexBinary0.dynamic = false;
    instance->hexBinaryPrefixed.dynamic = true;
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
    parse_le_int64(&instance->le_int46, 46, pstate);
    if (pstate->error) return;
    parse_le_uint16(&instance->le_uint16, 16, pstate);
    if (pstate->error) return;
    parse_le_uint32(&instance->le_uint32, 32, pstate);
    if (pstate->error) return;
    parse_le_uint64(&instance->le_uint64, 64, pstate);
    if (pstate->error) return;
    parse_le_uint8(&instance->le_uint8, 8, pstate);
    if (pstate->error) return;
    parse_le_uint16(&instance->le_uint10, 10, pstate);
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
    unparse_le_int64(instance->le_int46, 46, ustate);
    if (ustate->error) return;
    unparse_le_uint16(instance->le_uint16, 16, ustate);
    if (ustate->error) return;
    unparse_le_uint32(instance->le_uint32, 32, ustate);
    if (ustate->error) return;
    unparse_le_uint64(instance->le_uint64, 64, ustate);
    if (ustate->error) return;
    unparse_le_uint8(instance->le_uint8, 8, ustate);
    if (ustate->error) return;
    unparse_le_uint16(instance->le_uint10, 10, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinary0, ustate);
    if (ustate->error) return;
    unparse_le_uint16(instance->hexBinaryPrefixed.lengthInBytes, 16, ustate);
    if (ustate->error) return;
    unparse_hexBinary(instance->hexBinaryPrefixed, ustate);
    if (ustate->error) return;
}

static void
fixed_ex_nums__initERD(fixed_ex_nums_ *instance, InfosetBase *parent)
{
    instance->_base.erd = &fixed_ex_nums_ERD;
    instance->_base.parent = parent;
    instance->hexBinary_deadbeef.array = instance->_a_hexBinary_deadbeef;
    instance->hexBinary_deadbeef.lengthInBytes = sizeof(instance->_a_hexBinary_deadbeef);
    instance->hexBinary_deadbeef.dynamic = false;
    instance->hexBinary0.array = NULL;
    instance->hexBinary0.lengthInBytes = 0;
    instance->hexBinary0.dynamic = false;
    instance->hexBinaryPrefixed_ab.dynamic = true;
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
ex_nums__initERD(ex_nums_ *instance, InfosetBase *parent)
{
    instance->_base.erd = &ex_nums_ERD;
    instance->_base.parent = parent;
    array_ex_nums__initERD(&instance->array, (InfosetBase *)instance);
    bigEndian_ex_nums__initERD(&instance->bigEndian, (InfosetBase *)instance);
    littleEndian_ex_nums__initERD(&instance->littleEndian, (InfosetBase *)instance);
    fixed_ex_nums__initERD(&instance->fixed, (InfosetBase *)instance);
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

// Get an infoset (optionally clearing it first) for parsing/walking

InfosetBase *
get_infoset(bool clear_infoset)
{
    static ex_nums_ infoset;

    if (clear_infoset)
    {
        // If your infoset contains hexBinary prefixed length elements,
        // you may want to walk infoset first to free their malloc'ed
        // storage - we are not handling that case for now...
        memset(&infoset, 0, sizeof(infoset));
        ex_nums__initERD(&infoset, (InfosetBase *)&infoset);
    }

    return &infoset._base;
}
