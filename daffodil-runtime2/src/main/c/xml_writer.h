#ifndef XML_WRITER_H
#define XML_WRITER_H

#include "common_runtime.h"

// XMLWriter - infoset visitor with methods to output XML

typedef struct XMLWriter
{
    const VisitEventHandler handler;
    FILE *                  stream;
} XMLWriter;

// Write XML header to stream before writing anything else (probably
// should add this to visitor methods)

extern void xml_init(const XMLWriter *writer);

// XML visitor methods to pass to visit method

extern const VisitEventHandler xmlWriterMethods;

#endif // XML_WRITER_H
