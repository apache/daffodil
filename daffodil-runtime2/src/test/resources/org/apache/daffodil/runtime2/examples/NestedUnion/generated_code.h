#ifndef GENERATED_CODE_H
#define GENERATED_CODE_H

// clang-format off
#include "infoset.h"  // for HexBinary, InfosetBase
#include <stdbool.h>  // for bool
#include <stddef.h>   // for size_t
#include <stdint.h>   // for uint8_t, int16_t, int32_t, int64_t, uint32_t, int8_t, uint16_t, uint64_t
// clang-format on

// Define infoset structures

typedef struct foo
{
    InfosetBase _base;
    int32_t     a;
    int32_t     b;
    int32_t     c;
} foo;

typedef struct bar
{
    InfosetBase _base;
    double      x;
    double      y;
    double      z;
} bar;

typedef struct data
{
    InfosetBase _base;
    size_t      _choice; // choice of which union field to use
    union
    {
        foo foo;
        bar bar;
    };
} data;

typedef struct NestedUnion
{
    InfosetBase _base;
    int32_t     tag;
    data data;
} NestedUnion;

#endif // GENERATED_CODE_H
