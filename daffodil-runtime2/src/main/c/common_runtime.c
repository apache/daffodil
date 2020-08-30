#include "common_runtime.h"
#include <stdlib.h>

// walkInfosetNode - recursively walk an infoset node and call
// VisitEventHandler methods

static const char *
walkInfosetNode(const VisitEventHandler *handler, const InfosetBase *infoNode)
{
    const char *error_msg = NULL;

    // Start visiting the node
    if (error_msg == NULL)
    {
        error_msg = handler->visitStartComplex(handler, infoNode);
    }

    // Walk the node's children recursively
    const size_t      count = infoNode->erd->count_children;
    const ERD **const childrenERDs = infoNode->erd->childrenERDs;
    const ptrdiff_t * offsets = infoNode->erd->offsets;

    for (size_t i = 0; i < count && error_msg == NULL; i++)
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
            error_msg = walkInfosetNode(handler, childNode);
            break;
        case PRIMITIVE_INT:
            error_msg = handler->visitInt32Elem(handler, childERD, intLocation);
            break;
        }
    }

    // End visiting the node
    if (error_msg == NULL)
    {
        error_msg = handler->visitEndComplex(handler, infoNode);
    }

    return error_msg;
}

// walkInfoset - walk an infoset and call VisitEventHandler methods

const char *
walkInfoset(const VisitEventHandler *handler, const InfosetBase *infoset)
{
    const char *error_msg = NULL;

    if (error_msg == NULL)
    {
        error_msg = handler->visitStartDocument(handler);
    }
    if (error_msg == NULL)
    {
        error_msg = walkInfosetNode(handler, infoset);
    }
    if (error_msg == NULL)
    {
        error_msg = handler->visitEndDocument(handler);
    }

    return error_msg;
}
