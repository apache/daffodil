#ifndef XML_READER_H
#define XML_READER_H

#include "common_runtime.h" // for VisitEventHandler, InfosetBase
#include <mxml.h>           // for mxml_node_t
#include <stdio.h>          // for FILE

// XMLReader - infoset visitor with methods to read XML

typedef struct XMLReader
{
    const VisitEventHandler handler;
    FILE *                  stream;
    InfosetBase *           root;
    mxml_node_t *           xml;
    mxml_node_t *           node;
} XMLReader;

// XMLReader methods to pass to walkInfoset method

extern const VisitEventHandler xmlReaderMethods;

#endif // XML_READER_H
