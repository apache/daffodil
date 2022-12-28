#ifndef GENERATED_CODE_H
#define GENERATED_CODE_H

// clang-format off
#include <stdbool.h>  // for bool
#include <stddef.h>   // for size_t
#include <stdint.h>   // for uint8_t, int16_t, int32_t, int64_t, uint32_t, int8_t, uint16_t, uint64_t
#include "infoset.h"  // for InfosetBase, HexBinary
// clang-format on

// Define infoset structures

typedef struct expressionElement_
{
    InfosetBase _base;
    uint32_t    before;
    uint32_t    variablelen_size;
    uint32_t    variablelen[16];
    uint32_t    after[2];
} expressionElement_;

#endif // GENERATED_CODE_H
