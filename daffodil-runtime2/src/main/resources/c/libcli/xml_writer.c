/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "xml_writer.h"
#include "stack.h"    // for stack_is_empty, stack_pop, stack_push, stack_top, stack_init, stack_is_full
#include <assert.h>   // for assert
#include <mxml.h>     // for mxmlNewOpaquef, mxml_node_t, mxmlElementSetAttr, mxmlNewElement, mxmlDelete, mxmlNewXML, mxmlSaveFile, MXML_NO_CALLBACK
#include <stdbool.h>  // for bool
#include <stdint.h>   // for int16_t, int32_t, int64_t, int8_t, uint16_t, uint32_t, uint64_t, uint8_t
#include <stdio.h>    // for NULL, fflush
#include <string.h>   // for strcmp

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
    return xml ? NULL : "Error making new XML declaration";
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
        const char * name = get_erd_name(base->erd);
        const char * xmlns = get_erd_xmlns(base->erd);
        complex = mxmlNewElement(parent, name);
        if (xmlns)
        {
            const char *ns = get_erd_ns(base->erd);
            mxmlElementSetAttr(complex, xmlns, ns);
        }
        stack_push(&writer->stack, complex);
    }
    return complex ? NULL : "Error making new complex element";
}

// Pop completed complex element off stack

static const char *
xmlEndComplex(XMLWriter *writer, const InfosetBase *base)
{
    mxml_node_t *complex = NULL;
    if (!stack_is_empty(&writer->stack))
    {
        complex = stack_pop(&writer->stack);

        const char *name_from_xml = mxmlGetElement(complex);
        const char *name_from_erd = get_erd_name(base->erd);
        assert(strcmp(name_from_xml, name_from_erd) == 0);
    }
    return complex ? NULL : "Underflowed the XML stack";
}

// Fix a real number to conform to xsd:float syntax if needed

static void
fixNumberIfNeeded(const char *text)
{
    if (text[0] == 'N' && text[1] == 'A')
    {
        // xsd:float requires NaN to be capitalized correctly
        char *modifyInPlace = (char *)text;
        modifyInPlace[1] = 'a';
    }
    // These are not required by xsd:float, only to match runtime1 better
    //  - Strip + from <f>E+<e> to get <f>E<e> (not worth it)
    //  - Add .0 to 1 to get 1.0 (not worth it)
}

// Write a boolean, 32-bit or 64-bit real number, or 8, 16, 32, or
// 64-bit signed or unsigned integer as an XML element's value

static const char *
xmlNumberElem(XMLWriter *writer, const ERD *erd, const void *number)
{
    mxml_node_t *parent = stack_top(&writer->stack);
    const char * name = get_erd_name(erd);
    mxml_node_t *simple = mxmlNewElement(parent, name);

    // Set namespace declaration if necessary
    const char *xmlns = get_erd_xmlns(erd);
    if (xmlns)
    {
        const char *ns = get_erd_ns(erd);
        mxmlElementSetAttr(simple, xmlns, ns);
    }

    // Handle varying bit lengths of both signed & unsigned numbers
    const enum TypeCode typeCode = erd->typeCode;
    mxml_node_t *       text = NULL;
    switch (typeCode)
    {
    case PRIMITIVE_BOOLEAN:
        text = mxmlNewOpaquef(simple, "%s",
                              *(const bool *)number ? "true" : "false");
        break;
    case PRIMITIVE_FLOAT:
        // Round-trippable float, shortest possible
        text = mxmlNewOpaquef(simple, "%.9G", *(const float *)number);
        fixNumberIfNeeded(mxmlGetOpaque(text));
        break;
    case PRIMITIVE_DOUBLE:
        // Round-trippable double, shortest possible
        text = mxmlNewOpaquef(simple, "%.17lG", *(const double *)number);
        fixNumberIfNeeded(mxmlGetOpaque(text));
        break;
    case PRIMITIVE_INT16:
        text = mxmlNewOpaquef(simple, "%hi", *(const int16_t *)number);
        break;
    case PRIMITIVE_INT32:
        text = mxmlNewOpaquef(simple, "%i", *(const int32_t *)number);
        break;
    case PRIMITIVE_INT64:
        text = mxmlNewOpaquef(simple, "%li", *(const int64_t *)number);
        break;
    case PRIMITIVE_INT8:
        text = mxmlNewOpaquef(simple, "%hhi", *(const int8_t *)number);
        break;
    case PRIMITIVE_UINT16:
        text = mxmlNewOpaquef(simple, "%hu", *(const uint16_t *)number);
        break;
    case PRIMITIVE_UINT32:
        text = mxmlNewOpaquef(simple, "%u", *(const uint32_t *)number);
        break;
    case PRIMITIVE_UINT64:
        text = mxmlNewOpaquef(simple, "%lu", *(const uint64_t *)number);
        break;
    case PRIMITIVE_UINT8:
        text = mxmlNewOpaquef(simple, "%hhu", *(const uint8_t *)number);
        break;
    default:
        // Let text remain NULL and report error below
        break;
    }

    return (simple && text) ? NULL
                            : "Error making new simple numerical element";
}

// Initialize a struct with our visitor event handler methods

const VisitEventHandler xmlWriterMethods = {
    (VisitStartDocument)&xmlStartDocument, (VisitEndDocument)&xmlEndDocument,
    (VisitStartComplex)&xmlStartComplex,   (VisitEndComplex)&xmlEndComplex,
    (VisitNumberElem)&xmlNumberElem,
};
