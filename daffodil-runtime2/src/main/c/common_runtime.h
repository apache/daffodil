#ifndef COMMON_RUNTIME_H
#define COMMON_RUNTIME_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

// PState - parser state while parsing input

typedef struct PState
{
    FILE *stream; // input to read from
} PState;

// UState - unparser state while unparsing infoset

typedef struct UState
{
    FILE *stream; // output to write to
} UState;

// GlobalQName - name of an infoset element

typedef struct GlobalQName
{
    char *name;
} GlobalQName;

// TypeCode - type of an infoset element

enum TypeCode
{
    COMPLEX,
    PRIMITIVE_INT
};

// ERD - element runtime data needed to parse/unparse objects

typedef struct ElementRuntimeData ERD;
typedef struct InfosetBase        InfosetBase;
typedef void (*Init_Self)(InfosetBase *infoNode);
typedef const char *(*Parse_Self)(InfosetBase *infoNode, const PState *pstate);
typedef const char *(*Unparse_Self)(const InfosetBase *infoNode,
                                    const UState *     ustate);

typedef struct ElementRuntimeData
{
    const GlobalQName   namedQName;
    const enum TypeCode typeCode;
    const size_t        count_children;
    const ptrdiff_t *   offsets;
    const ERD **        childrenERDs;

    const Init_Self    initSelf;
    const Parse_Self   parseSelf;
    const Unparse_Self unparseSelf;
} ERD;

// InfosetBase - representation of an infoset element

typedef struct InfosetBase
{
    const ERD *erd;
} InfosetBase;

// VisitEventHandler - methods to be called when walking an infoset

typedef struct VisitEventHandler VisitEventHandler;
typedef const char *(*VisitStartDocument)(const VisitEventHandler *handler);
typedef const char *(*VisitEndDocument)(const VisitEventHandler *handler);
typedef const char *(*VisitStartComplex)(const VisitEventHandler *handler,
                                         const InfosetBase *      base);
typedef const char *(*VisitEndComplex)(const VisitEventHandler *handler,
                                       const InfosetBase *      base);
typedef const char *(*VisitInt32Elem)(const VisitEventHandler *handler,
                                      const ERD *erd, const int32_t *location);

typedef struct VisitEventHandler
{
    const VisitStartDocument visitStartDocument;
    const VisitEndDocument   visitEndDocument;
    const VisitStartComplex  visitStartComplex;
    const VisitEndComplex    visitEndComplex;
    const VisitInt32Elem     visitInt32Elem;
} VisitEventHandler;

// walkInfoset - walk an infoset and call VisitEventHandler methods

extern const char *walkInfoset(const VisitEventHandler *handler,
                               const InfosetBase *      infoset);

#endif // COMMON_RUNTIME_H
