#include <stdlib.h>
#include "common_runtime.h"

// PState - parser state while parsing input

PState *new_pstate(FILE *stream)
{
	PState *pstate = malloc(sizeof(PState));
	pstate->stream = stream;
	return pstate;
}

// UState - unparser state while parsing infoset

UState *new_ustate(FILE *stream)
{
	UState *ustate = malloc(sizeof(UState));
	ustate->stream = stream;
	return ustate;
}

// GlobalQName - name of an infoset element

GlobalQName *new_qname(char *name)
{
	GlobalQName *qname = malloc(sizeof(GlobalQName));
	qname->name = name;
	return qname;
}

// ERD - element runtime data needed to parse/unparse objects
// Constructors will be in generated code, not generic runtime.

// Generic methods to visit infoset objects with a visit handler

void visit_node_self(VisitEventHandler *handler, InfosetBase *infoNode)
{
	handler->visitStart(handler, infoNode);
	// Iterate through children...
	int count = infoNode->erd->count_children;
	ERD *childERD = infoNode->erd->childrenERD;
	size_t *p_offset = infoNode->erd->offsets;
	for (int i = 0; i < count; i++)
	{
		size_t offset = *p_offset;
		// NOTE: This can't be right - both childNode and intLocation get the same value
		InfosetBase *childNode = (InfosetBase *)((char *)infoNode + offset);
		int *intLocation = (int *)((char *)infoNode + offset);

		// Need to handle more element types...
		enum TypeCode typeCode = childERD->typeCode;
		switch (typeCode)
		{
		case COMPLEX:
			visit_node_self(handler, childNode);
			break;
		case PRIMITIVE_INT:
			handler->visitInt(handler, childERD, intLocation);
			break;
		}
		p_offset++;
		childERD++;
	}
	handler->visitEnd(handler, infoNode);
}
