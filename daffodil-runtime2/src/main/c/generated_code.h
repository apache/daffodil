#ifndef GENERATED_CODE_H
#define GENERATED_CODE_H

#include "common_runtime.h" // for InfosetBase
#include <stdint.h>         // for int32_t

// Return the root of an infoset to be used for parsing or unparsing

extern InfosetBase *rootInfoset();

// Define some infoset structures

typedef struct C2
{
    InfosetBase _base;
    int32_t     e2;
    int32_t     e3;
} C2;

typedef struct C1
{
    InfosetBase _base;
    C2          c2;
    int32_t     e1;
} C1;

#endif // GENERATED_CODE_H
