#ifndef GENERATED_CODE_H
#define GENERATED_CODE_H

// auto-maintained by iwyu
// clang-format off
#include <stdbool.h>  // for bool
#include <stddef.h>   // for size_t
#include <stdint.h>   // for uint8_t, int16_t, int32_t, int64_t, uint32_t, int8_t, uint16_t, uint64_t
#include "infoset.h"  // for InfosetBase, HexBinary
// clang-format on

// Define schema version (will be empty if schema did not define any version string)

extern const char *schema_version;

// Define infoset structures

typedef struct simple_
{
    InfosetBase _base;
    bool        simple_boolean;
    int8_t      simple_byte;
    double      simple_double;
    float       simple_float;
    HexBinary   simple_hexBinary;
    uint8_t     _a_simple_hexBinary[4];
    HexBinary   simple_hexBinaryPrefixed;
    int32_t     simple_int;
    int32_t     simple_integer;
    int64_t     simple_long;
    uint32_t    simple_nonNegativeInteger;
    int16_t     simple_short;
    uint8_t     simple_unsignedByte;
    uint32_t    simple_unsignedInt;
    uint64_t    simple_unsignedLong;
    uint16_t    simple_unsignedShort;
    int8_t      enum_byte;
    double      enum_double;
    float       enum_float;
    HexBinary   enum_hexBinary;
    uint8_t     _a_enum_hexBinary[4];
    HexBinary   enum_hexBinaryPrefixed;
    int32_t     enum_int;
    int32_t     enum_integer;
    int64_t     enum_long;
    uint32_t    enum_nonNegativeInteger;
    int16_t     enum_short;
    uint8_t     enum_unsignedByte;
    uint32_t    enum_unsignedInt;
    uint64_t    enum_unsignedLong;
    uint16_t    enum_unsignedShort;
    int8_t      range_byte;
    double      range_double;
    float       range_float;
    int32_t     range_int;
    int32_t     range_integer;
    int64_t     range_long;
    uint32_t    range_nonNegativeInteger;
    int16_t     range_short;
    uint8_t     range_unsignedByte;
    uint32_t    range_unsignedInt;
    uint64_t    range_unsignedLong;
    uint16_t    range_unsignedShort;
} simple_;

#endif // GENERATED_CODE_H
