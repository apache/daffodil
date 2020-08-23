#include "common_runtime.h"
#include <stdlib.h>

// Generic method to visit infoset objects with a visit handler

void
visit_node_self(const VisitEventHandler *handler, const InfosetBase *infoNode)
{
    const size_t      count = infoNode->erd->count_children;
    const ERD **const childrenERDs = infoNode->erd->childrenERDs;
    const ptrdiff_t * offsets = infoNode->erd->offsets;

    handler->visitStart(handler, infoNode);

    // Visit each child too
    for (size_t i = 0; i < count; i++)
    {
        ptrdiff_t  offset = offsets[i];
        const ERD *childERD = childrenERDs[i];
        // We use only one of these variables below depending on typeCode
        const InfosetBase *childNode =
            (const InfosetBase *)((const char *)infoNode + offset);
        const int32_t *intLocation =
            (const int32_t *)((const char *)infoNode + offset);

        // Need to handle more element types
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
    }

    handler->visitEnd(handler, infoNode);
}
