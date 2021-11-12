#ifndef GENERATED_CODE_H
#define GENERATED_CODE_H

// clang-format off
#include <stdbool.h>  // for bool
#include <stddef.h>   // for size_t
#include <stdint.h>   // for uint8_t, int16_t, int32_t, int64_t, uint32_t, int8_t, uint16_t, uint64_t
#include "infoset.h"  // for InfosetBase, HexBinary
// clang-format on

// Define infoset structures

typedef struct array
{
    InfosetBase _base;
    bool        be_bool16[2];
    float       be_float[3];
    int16_t     be_int16[3];
    HexBinary   hexBinary2[3];
    uint8_t     _a_hexBinary2[3][2];
    HexBinary   hexBinaryPrefixed[3];
} array;

typedef struct bigEndian
{
    InfosetBase _base;
    bool        be_bool16;
    bool        be_bool32;
    bool        be_bool8;
    bool        be_boolean;
    double      be_double;
    float       be_float;
    int16_t     be_int16;
    int32_t     be_int32;
    int64_t     be_int64;
    int8_t      be_int8;
    int16_t     be_integer16;
    uint16_t    be_uint16;
    uint32_t    be_uint32;
    uint64_t    be_uint64;
    uint8_t     be_uint8;
    uint32_t    be_nonNegativeInteger32;
    HexBinary   hexBinary4;
    uint8_t     _a_hexBinary4[4];
    HexBinary   hexBinaryPrefixed;
} bigEndian;

typedef struct littleEndian
{
    InfosetBase _base;
    bool        le_bool16;
    bool        le_bool32;
    bool        le_bool8;
    bool        le_boolean;
    double      le_double;
    float       le_float;
    int16_t     le_int16;
    int32_t     le_int32;
    int64_t     le_int64;
    int8_t      le_int8;
    int64_t     le_integer64;
    uint16_t    le_uint16;
    uint32_t    le_uint32;
    uint64_t    le_uint64;
    uint8_t     le_uint8;
    uint8_t     le_nonNegativeInteger8;
    HexBinary   hexBinary0;
    HexBinary   hexBinaryPrefixed;
} littleEndian;

typedef struct fixed
{
    InfosetBase _base;
    bool        boolean_false;
    bool        boolean_true;
    float       float_1_5;
    int32_t     int_32;
    HexBinary   hexBinary_deadbeef;
    uint8_t     _a_hexBinary_deadbeef[4];
    HexBinary   hexBinary0;
    HexBinary   hexBinaryPrefixed_ab;
} fixed;

typedef struct ex_nums
{
    InfosetBase _base;
    array array;
    bigEndian bigEndian;
    littleEndian littleEndian;
    fixed fixed;
} ex_nums;

#endif // GENERATED_CODE_H
