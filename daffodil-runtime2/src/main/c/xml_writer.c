#include "xml_writer.h"
#include <stdio.h>

// Write XML header to stream before writing anything else (probably
// should add this to visitor methods)

void
xml_init(const XMLWriter *writer)
{
    fprintf(writer->stream, u8"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
}

// Implement visitor methods to write infoset nodes to stream as XML

static void
xml_start(const XMLWriter *writer, const InfosetBase *base)
{
    char *name = base->erd->namedQName.name;
    fprintf(writer->stream, u8"<%s>\n", name);
}

static void
xml_end(const XMLWriter *writer, const InfosetBase *base)
{
    char *name = base->erd->namedQName.name;
    fprintf(writer->stream, u8"</%s>\n", name);
}

static void
xml_int(const XMLWriter *writer, const ERD *erd, const int32_t *location)
{
    char *  name = erd->namedQName.name;
    int32_t value = *location;
    fprintf(writer->stream, u8"  <%s>%i</%s>\n", name, value, name);
}

// List our visitor methods

const VisitEventHandler xmlWriterMethods = {
    (VisitStart)&xml_start, (VisitEnd)&xml_end, (VisitInt)&xml_int};
