#include "xml_writer.h"
#include <stdio.h>

// Write XML declaration to stream before walking infoset

static const char *
xmlStartDocument(const XMLWriter *writer)
{
    int count = fprintf(writer->stream,
                        u8"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    return count > 0 ? NULL : "Error writing XML declaration";
}

// Flush stream after walking infoset

static const char *
xmlEndDocument(const XMLWriter *writer)
{
    int status = fflush(writer->stream);
    return status == 0 ? NULL : "Error flushing stream";
}

// Write node's name as start tag

static const char *
xmlStartComplex(const XMLWriter *writer, const InfosetBase *base)
{
    char *name = base->erd->namedQName.name;
    int   count = fprintf(writer->stream, u8"<%s>\n", name);
    return count > 0 ? NULL : "Error writing start tag";
}

// Write node's name as end tag

static const char *
xmlEndComplex(const XMLWriter *writer, const InfosetBase *base)
{
    char *name = base->erd->namedQName.name;
    int   count = fprintf(writer->stream, u8"</%s>\n", name);
    return count > 0 ? NULL : "Error writing end tag";
}

// Write 32-bit integer value as element

static const char *
xmlInt32Elem(const XMLWriter *writer, const ERD *erd, const int32_t *location)
{
    char *  name = erd->namedQName.name;
    int32_t value = *location;
    int count = fprintf(writer->stream, u8"  <%s>%i</%s>\n", name, value, name);
    return count > 0 ? NULL : "Error writing int32 element";
}

// Initialize a struct with our visitor event handler methods

const VisitEventHandler xmlWriterMethods = {
    (VisitStartDocument)&xmlStartDocument, (VisitEndDocument)&xmlEndDocument,
    (VisitStartComplex)&xmlStartComplex,   (VisitEndComplex)&xmlEndComplex,
    (VisitInt32Elem)&xmlInt32Elem,
};
