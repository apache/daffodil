#ifndef XML_WRITER_H
#define XML_WRITER_H

#include "common_runtime.h"

// XMLWriter - infoset visitor with methods to output XML

typedef struct XMLWriter
{
	VisitEventHandler handler;
	FILE *stream;
} XMLWriter;

extern VisitEventHandler xmlWriterMethods;

void xml_init(XMLWriter *writer);

#endif // XML_WRITER_H
