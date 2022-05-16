#ifndef GENERATED_CODE_H
#define GENERATED_CODE_H

// clang-format off
#include <stdbool.h>  // for bool
#include <stddef.h>   // for size_t
#include <stdint.h>   // for uint8_t, int16_t, int32_t, int64_t, uint32_t, int8_t, uint16_t, uint64_t
#include "infoset.h"  // for InfosetBase, HexBinary
// clang-format on

// Define infoset structures

typedef struct array_ex_nums_
{
    InfosetBase _base;
    bool        be_boolean[2];
    float       be_float[3];
    int16_t     be_int16[3];
    HexBinary   hexBinary2[3];
    uint8_t     _a_hexBinary2[3][2];
    HexBinary   hexBinaryPrefixed[3];
} array_ex_nums_;

typedef struct bigEndian_ex_nums_
{
    InfosetBase _base;
    bool        be_bool16;
    bool        be_boolean;
    double      be_double;
    float       be_float;
    int16_t     be_int16;
    int32_t     be_int32;
    int64_t     be_int64;
    int8_t      be_int8;
    int32_t     be_integer17;
    uint16_t    be_uint16;
    uint32_t    be_uint32;
    uint64_t    be_uint64;
    uint8_t     be_uint8;
    uint32_t    be_nonNegativeInteger31;
    HexBinary   hexBinary4;
    uint8_t     _a_hexBinary4[4];
    HexBinary   hexBinaryPrefixed;
} bigEndian_ex_nums_;

typedef struct littleEndian_ex_nums_
{
    InfosetBase _base;
    bool        le_bool16;
    bool        le_boolean;
    double      le_double;
    float       le_float;
    int16_t     le_int16;
    int32_t     le_int32;
    int64_t     le_int64;
    int8_t      le_int8;
    int64_t     le_integer46;
    uint16_t    le_uint16;
    uint32_t    le_uint32;
    uint64_t    le_uint64;
    uint8_t     le_uint8;
    uint16_t    le_nonNegativeInteger10;
    HexBinary   hexBinary0;
    HexBinary   hexBinaryPrefixed;
} littleEndian_ex_nums_;

typedef struct fixed_ex_nums_
{
    InfosetBase _base;
    bool        boolean_false;
    bool        boolean_true;
    double      double_3;
    float       float_1_5;
    int32_t     int_32;
    HexBinary   hexBinary_deadbeef;
    uint8_t     _a_hexBinary_deadbeef[4];
    HexBinary   hexBinary0;
    HexBinary   hexBinaryPrefixed_ab;
} fixed_ex_nums_;

typedef struct ex_nums_
{
    InfosetBase _base;
    array_ex_nums_ array;
    bigEndian_ex_nums_ bigEndian;
    littleEndian_ex_nums_ littleEndian;
    fixed_ex_nums_ fixed;
} ex_nums_;

#endif // GENERATED_CODE_H
