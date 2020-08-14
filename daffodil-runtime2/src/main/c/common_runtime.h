#ifndef COMMON_RUNTIME_H
#define COMMON_RUNTIME_H

#include <stdbool.h>
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
typedef struct InfosetBase InfosetBase;
typedef void (*Parse_Self)(InfosetBase *infoNode, PState *pstate);
typedef void (*Unparse_Self)(InfosetBase *infoNode, UState *ustate);
typedef void (*Init_Self)(InfosetBase *infoNode);

typedef struct ElementRuntimeData
{
	GlobalQName namedQName;
	enum TypeCode typeCode;
	uint32_t count_children;
	size_t *offsets;
	ERD **childrenERDs;

	Parse_Self parseSelf;
	Unparse_Self unparseSelf;
	Init_Self initSelf;
} ERD;

// VisitEventHandler - infoset visitor methods (generic)

typedef struct VisitEventHandler VisitEventHandler;
typedef void (*VisitStart)(VisitEventHandler *handler, InfosetBase *base);
typedef void (*VisitEnd)(VisitEventHandler *handler, InfosetBase *base);
typedef void (*VisitInt)(VisitEventHandler *handler, ERD *erd, int *location);

typedef struct VisitEventHandler
{
	VisitStart visitStart;
	VisitEnd visitEnd;
	VisitInt visitInt;
} VisitEventHandler;

// InfosetBase - representation of an infoset element

typedef struct InfosetBase
{
	ERD *erd;
} InfosetBase;

// Generic visit method

void visit_node_self(VisitEventHandler *handler, InfosetBase *infoNode);

#endif // COMMON_RUNTIME_H
