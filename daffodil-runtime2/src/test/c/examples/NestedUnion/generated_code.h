#ifndef GENERATED_CODE_H
#define GENERATED_CODE_H

// clang-format off
#include <stdbool.h>  // for bool
#include <stddef.h>   // for size_t
#include <stdint.h>   // for uint8_t, int16_t, int32_t, int64_t, uint32_t, int8_t, uint16_t, uint64_t
#include "infoset.h"  // for InfosetBase, HexBinary
// clang-format on

// Define infoset structures

typedef struct foo_data_NestedUnionType_
{
    InfosetBase _base;
    int32_t     a;
    int32_t     b;
    int32_t     c;
} foo_data_NestedUnionType_;

typedef struct bar_data_NestedUnionType_
{
    InfosetBase _base;
    double      x;
    double      y;
    double      z;
} bar_data_NestedUnionType_;

typedef struct data_NestedUnionType_
{
    InfosetBase _base;
    size_t      _choice; // choice of which union field to use
    union
    {
        foo_data_NestedUnionType_ foo;
        bar_data_NestedUnionType_ bar;
    };
} data_NestedUnionType_;

typedef struct NestedUnion_
{
    InfosetBase _base;
    int32_t     tag;
    data_NestedUnionType_ data;
} NestedUnion_;

#endif // GENERATED_CODE_H
