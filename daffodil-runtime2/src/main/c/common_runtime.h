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

// VisitEventHandler - infoset visitor methods (generic)

typedef struct VisitEventHandler VisitEventHandler;
typedef void (*VisitStart)(const VisitEventHandler *handler,
                           const InfosetBase *      base);
typedef void (*VisitEnd)(const VisitEventHandler *handler,
                         const InfosetBase *      base);
typedef void (*VisitInt)(const VisitEventHandler *handler, const ERD *erd,
                         const int32_t *location);

typedef struct VisitEventHandler
{
    const VisitStart visitStart;
    const VisitEnd   visitEnd;
    const VisitInt   visitInt;
} VisitEventHandler;

// InfosetBase - representation of an infoset element

typedef struct InfosetBase
{
    const ERD *erd;
} InfosetBase;

// Generic visit method

extern void visit_node_self(const VisitEventHandler *handler,
                            const InfosetBase *      infoNode);

#endif // COMMON_RUNTIME_H
