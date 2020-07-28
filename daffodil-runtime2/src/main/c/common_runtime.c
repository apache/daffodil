#include <stdlib.h>
#include "common_runtime.h"

// Generic method to visit infoset objects with a visit handler

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
