#ifndef GENERATED_CODE_H
#define GENERATED_CODE_H

#include "common_runtime.h" // for InfosetBase
#include <stdint.h>         // for int32_t

// Return the root of an infoset to be used for parsing or unparsing

extern InfosetBase *rootInfoset();

// Define some infoset structures

typedef struct c2
{
    InfosetBase _base;
    int32_t     e2;
    int32_t     e3;
} c2;

typedef struct c1
{
    InfosetBase _base;
    c2          c2;
    int32_t     e1;
} c1;

#endif // GENERATED_CODE_H
