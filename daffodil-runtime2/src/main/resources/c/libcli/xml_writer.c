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
#include <assert.h>   // for assert
#include <mxml.h>     // for mxmlNewOpaquef, mxml_node_t, mxmlElementSetAttr, mxmlGetOpaque, mxmlNewElement, mxmlDelete, mxmlGetElement, mxmlNewXML, mxmlSaveFile, MXML_NO_CALLBACK
#include <stdbool.h>  // for bool
#include <stdint.h>   // for int16_t, int32_t, int64_t, int8_t, uint16_t, uint32_t, uint64_t, uint8_t
#include <stdio.h>    // for NULL
#include <string.h>   // for strcmp
#include "errors.h"   // for Error, ERR_XML_DECL, ERR_XML_ELEMENT, ERR_XML_WRITE, Error::(anonymous)
#include "stack.h"    // for stack_is_empty, stack_pop, stack_push, stack_top, stack_init

// Push new XML document on stack (note the stack is stored in a
// static array which could overflow and stop the program; it also
// means none of those functions are thread-safe)

static const Error *
xmlStartDocument(XMLWriter *writer)
{
    enum
    {
        MAX_DEPTH = 100
    };
    static mxml_node_t *array[MAX_DEPTH];
    stack_init(&writer->stack, array, MAX_DEPTH);

    mxml_node_t *xml = mxmlNewXML("1.0");
    if (xml)
    {
        stack_push(&writer->stack, xml);
        return NULL;
    }
    else
    {
        static Error error = {ERR_XML_DECL, {NULL}};
        return &error;
    }
}

// Pop completed XML document off stack and write it to stream (note
// stack underflow will stop program)

static const Error *
xmlEndDocument(XMLWriter *writer)
{
    mxml_node_t *xml = stack_pop(&writer->stack);
    assert(stack_is_empty(&writer->stack));

    int status = mxmlSaveFile(xml, writer->stream, MXML_NO_CALLBACK);
    if (status < 0)
    {
        static Error error = {ERR_XML_WRITE, {NULL}};
        return &error;
    }
    mxmlDelete(xml);
    return NULL;
}

// Push new complex element on stack (note stack overflow will stop
// program)

static const Error *
xmlStartComplex(XMLWriter *writer, const InfosetBase *base)
{
    mxml_node_t *parent = stack_top(&writer->stack);
    const char * name = get_erd_name(base->erd);
    const char * xmlns = get_erd_xmlns(base->erd);
    mxml_node_t *complex = mxmlNewElement(parent, name);
    if (xmlns)
    {
        const char *ns = get_erd_ns(base->erd);
        mxmlElementSetAttr(complex, xmlns, ns);
    }
    stack_push(&writer->stack, complex);
    return NULL;
}

// Pop completed complex element off stack (note stack underflow will
// stop program)

static const Error *
xmlEndComplex(XMLWriter *writer, const InfosetBase *base)
{
    mxml_node_t *complex = stack_pop(&writer->stack);

    const char *name_from_xml = mxmlGetElement(complex);
    const char *name_from_erd = get_erd_name(base->erd);
    assert(strcmp(name_from_xml, name_from_erd) == 0);

    return NULL;
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
    //  - Strip + from <f>E+<e> to get <f>E<e>
    //  - Add .0 to 1 to get 1.0
    // It would be better to compare floats as numbers, not strings, though
}

// Write a boolean, 32-bit or 64-bit real number, or 8, 16, 32, or
// 64-bit signed or unsigned integer as an XML element's value

static const Error *
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

    if (simple && text)
    {
        return NULL;
    }
    else
    {
        static Error error = {ERR_XML_ELEMENT, {NULL}};
        error.s = name;
        return &error;
    }
}

// Initialize a struct with our visitor event handler methods

const VisitEventHandler xmlWriterMethods = {
    (VisitStartDocument)&xmlStartDocument, (VisitEndDocument)&xmlEndDocument,
    (VisitStartComplex)&xmlStartComplex,   (VisitEndComplex)&xmlEndComplex,
    (VisitNumberElem)&xmlNumberElem,
};
