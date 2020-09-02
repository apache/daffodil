#include "xml_writer.h"
#include "stack.h"  // for stack_is_empty, stack_pop, stack_push, stack_top
#include <assert.h> // for assert
#include <mxml.h>   // for mxml_node_t, mxmlNewElement, mxmlNewOpaquef, ...
#include <stdint.h> // for int32_t
#include <stdio.h>  // for NULL, fflush

// Push new XML document on stack.  This function is not
// thread-safe since it uses static storage.

static const char *
xmlStartDocument(XMLWriter *writer)
{
#define MAX_DEPTH 100
    static mxml_node_t *array[MAX_DEPTH];
    stack_init(&writer->stack, array, MAX_DEPTH);

    mxml_node_t *xml = mxmlNewXML("1.0");
    stack_push(&writer->stack, xml);
    return xml != NULL ? NULL : "Error making new XML declaration";
}

// Pop completed XML document off stack and write it to stream

static const char *
xmlEndDocument(XMLWriter *writer)
{
    mxml_node_t *xml = stack_pop(&writer->stack);
    assert(stack_is_empty(&writer->stack));
    int status = mxmlSaveFile(xml, writer->stream, MXML_NO_CALLBACK);
    if (status < 0)
    {
        return "Error writing XML document";
    }
    status = fflush(writer->stream);
    mxmlDelete(xml);
    return status == 0 ? NULL : "Error flushing stream";
}

// Push new complex element on stack

static const char *
xmlStartComplex(XMLWriter *writer, const InfosetBase *base)
{
    mxml_node_t *complex = NULL;
    if (!stack_is_full(&writer->stack))
    {
        mxml_node_t *parent = stack_top(&writer->stack);
        char *       name = base->erd->namedQName.name;
        complex = mxmlNewElement(parent, name);
        stack_push(&writer->stack, complex);
    }
    return complex != NULL ? NULL : "Error making new complex element";
}

// Pop completed complex element off stack

static const char *
xmlEndComplex(XMLWriter *writer, const InfosetBase *base)
{
    mxml_node_t *complex = NULL;
    if (!stack_is_empty(&writer->stack))
    {
        complex = stack_pop(&writer->stack);
        (void)base;
    }
    return complex != NULL ? NULL : "Underflowed the XML stack";
}

// Write 32-bit integer value as element

static const char *
xmlInt32Elem(XMLWriter *writer, const ERD *erd, const int32_t *location)
{
    mxml_node_t *parent = stack_top(&writer->stack);
    char *       name = erd->namedQName.name;
    mxml_node_t *simple = mxmlNewElement(parent, name);
    int32_t      value = *location;
    mxml_node_t *text = mxmlNewOpaquef(simple, "%i", value);
    return (simple != NULL && text != NULL)
               ? NULL
               : "Error making new int32 simple element";
}

// Initialize a struct with our visitor event handler methods

const VisitEventHandler xmlWriterMethods = {
    (VisitStartDocument)&xmlStartDocument, (VisitEndDocument)&xmlEndDocument,
    (VisitStartComplex)&xmlStartComplex,   (VisitEndComplex)&xmlEndComplex,
    (VisitInt32Elem)&xmlInt32Elem,
};
