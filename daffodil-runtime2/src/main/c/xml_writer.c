#include <stdio.h>
#include "xml_writer.h"

void xml_init(XMLWriter *writer)
{
	fprintf(writer->stream, u8"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n");
}

void xml_start(XMLWriter *writer, InfosetBase *base)
{
	char* name = base->erd->namedQName->name;
	fprintf(writer->stream, u8"<%s>\n", name);
}

void xml_end(XMLWriter *writer, InfosetBase *base)
{
	char* name = base->erd->namedQName->name;
	fprintf(writer->stream, u8"</%s>\n", name);
}

void xml_int(XMLWriter *writer, ERD *erd, int *location)
{
	char* name = erd->namedQName->name;
	int value = *location;
	fprintf(writer->stream, u8"  <%s>%i</%s>\n", name, value, name);
}

VisitEventHandler xmlWriterMethods = {
	(VisitStart)&xml_start,
	(VisitEnd)&xml_end,
	(VisitInt)&xml_int};
