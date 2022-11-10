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

// clang-format off
#include "xml_writer.h"
#include <assert.h>      // for assert
#include <inttypes.h>    // for PRIi64, PRIu64
#include <mxml.h>        // for mxmlNewOpaquef, mxml_node_t, mxmlElementSetAttr, mxmlGetOpaque, mxmlNewElement, mxmlDelete, mxmlGetElement, mxmlNewOpaque, mxmlNewXML, mxmlSaveFile, MXML_NO_CALLBACK
#include <stdbool.h>     // for bool, false, true
#include <stdint.h>      // for uint8_t, int16_t, int32_t, int64_t, int8_t, uint16_t, uint32_t, uint64_t
#include <stdlib.h>      // for free, malloc
#include <string.h>      // for strcmp, strlen
#include "cli_errors.h"  // for CLI_XML_DECL, CLI_XML_ELEMENT, CLI_XML_WRITE, LIMIT_XML_NESTING
#include "errors.h"      // for Error, Error::(anonymous)
#include "stack.h"       // for stack_is_empty, stack_pop, stack_push, stack_top, stack_init
// clang-format on

// Fix a real number's syntax to remove as many textual differences
// between runtime1/runtime2 as possible.  A better approach may be
// to make TDMLRunner compare floats and doubles in XML numerically
// with a small epsilon instead of textually character by character.

static void
fixNumberIfNeeded(const char *text)
{
    // Change 'NAN' to 'NaN' to conform to xsd:float syntax
    if (text && text[0] == 'N' && text[1] == 'A')
    {
        char *modifyInPlace = (char *)text;
        modifyInPlace[1] = 'a';
    }

    // Chop trailing zeros except for one zero after dot
    size_t zero = strlen(text);
    while (zero && text[zero - 1] == '0')
        zero--;
    if (zero && text[zero - 1] == '.' && text[zero] == '0') zero++;
    if (zero && text[zero] == '0')
    {
        char *modifyInPlace = (char *)text;
        modifyInPlace[zero] = 0;
    }

    // Chop + from <f>E+<e> to match runtime2's <f>E<e> syntax
    size_t plus = 0;
    size_t i = 0;
    while (text[i++])
    {
        if (text[i - 1] == 'E' && text[i] == '+')
        {
            plus = i;
            break;
        }
    }
    while (plus && plus < zero)
    {
        char *modifyInPlace = (char *)text;
        modifyInPlace[plus] = text[plus + 1];
        plus++;
    }
}

// Convert a byte array to a string of hexadecimal characters (two
// nibbles per byte).  Return NULL if no dynamic memory could be
// allocated for string.  Reuse same dynamic memory next time if big
// enough.  Caller is responsible for copying string before next call
// and calling with freeMemory true to release dynamic memory at end.

static const char *
binaryToHex(HexBinary hexBinary, bool freeMemory)
{
    static char * text = NULL;
    static size_t capacity = 256;

    // Call with freeMemory true when finished
    if (freeMemory)
    {
        free(text);
        text = NULL;
        capacity = 256;
        return NULL;
    }

    // Make room for hexadecimal characters if necessary
    size_t numNibbles = hexBinary.lengthInBytes * 2;
    if (text == NULL || capacity < numNibbles)
    {
        free(text);
        capacity = numNibbles > capacity ? numNibbles : capacity;
        text = malloc(capacity + 1);
    }

    // Check whether dynamic memory allocation succeeded
    if (text == NULL)
    {
        return NULL;
    }

    // Convert each binary byte to two hexadecimal characters
    char *nibble = text;
    for (size_t i = 0; i < hexBinary.lengthInBytes; i++)
    {
        static char hexDigit[] = "0123456789ABCDEF";
        *(nibble++) = hexDigit[hexBinary.array[i] / 16]; // high nibble
        *(nibble++) = hexDigit[hexBinary.array[i] % 16]; // low nibble
    }
    *(nibble) = '\0';

    return text;
}

// Push new XML document on stack (note the stack is stored in a
// static array which could overflow and stop the program; it also
// means none of those functions are thread-safe)

static const Error *
xmlStartDocument(XMLWriter *writer)
{
    static mxml_node_t *array[LIMIT_XML_NESTING];
    stack_init(&writer->stack, array, LIMIT_XML_NESTING);

    mxml_node_t *xml = mxmlNewXML("1.0");
    if (xml)
    {
        stack_push(&writer->stack, xml);
        return NULL;
    }
    else
    {
        static Error error = {CLI_XML_DECL, {0}};
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
        static Error error = {CLI_XML_WRITE, {0}};
        return &error;
    }

    // Free memory allocated by binaryToHex and mxml functions
    HexBinary hexBinary = {NULL, 0, false};
    (void)binaryToHex(hexBinary, true);
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

// Write a boolean, 32-bit or 64-bit real number, hexBinary, or
// 8, 16, 32, or 64-bit signed or unsigned integer as an XML element's text

static const Error *
xmlSimpleElem(XMLWriter *writer, const ERD *erd, const void *valueptr)
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

    // Format various types of values as XML element's text
    mxml_node_t *       text = NULL;
    const enum TypeCode typeCode = erd->typeCode;
    switch (typeCode)
    {
    case PRIMITIVE_BOOLEAN:
        text = mxmlNewOpaquef(simple, "%s", *(const bool *)valueptr ? "true" : "false");
        break;
    case PRIMITIVE_FLOAT:
        // Format as float with same precision as runtime1
        text = mxmlNewOpaquef(simple, "%#.8G", *(const float *)valueptr);
        fixNumberIfNeeded(mxmlGetOpaque(text));
        break;
    case PRIMITIVE_DOUBLE:
        // Format as double with same precision as runtime1
        text = mxmlNewOpaquef(simple, "%#.16lG", *(const double *)valueptr);
        fixNumberIfNeeded(mxmlGetOpaque(text));
        break;
    case PRIMITIVE_HEXBINARY:
        text = mxmlNewOpaque(simple, binaryToHex(*(const HexBinary *)valueptr, false));
        break;
    case PRIMITIVE_INT16:
        text = mxmlNewOpaquef(simple, "%hi", *(const int16_t *)valueptr);
        break;
    case PRIMITIVE_INT32:
        text = mxmlNewOpaquef(simple, "%i", *(const int32_t *)valueptr);
        break;
    case PRIMITIVE_INT64:
        text = mxmlNewOpaquef(simple, "%" PRIi64, *(const int64_t *)valueptr);
        break;
    case PRIMITIVE_INT8:
        text = mxmlNewOpaquef(simple, "%hhi", *(const int8_t *)valueptr);
        break;
    case PRIMITIVE_UINT16:
        text = mxmlNewOpaquef(simple, "%hu", *(const uint16_t *)valueptr);
        break;
    case PRIMITIVE_UINT32:
        text = mxmlNewOpaquef(simple, "%u", *(const uint32_t *)valueptr);
        break;
    case PRIMITIVE_UINT64:
        text = mxmlNewOpaquef(simple, "%" PRIu64, *(const uint64_t *)valueptr);
        break;
    case PRIMITIVE_UINT8:
        text = mxmlNewOpaquef(simple, "%hhu", *(const uint8_t *)valueptr);
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
        static Error error = {CLI_XML_ELEMENT, {0}};
        error.arg.s = name;
        return &error;
    }
}

// Initialize a struct with our visitor event handler methods

const VisitEventHandler xmlWriterMethods = {
    (VisitStartDocument)&xmlStartDocument, (VisitEndDocument)&xmlEndDocument,
    (VisitStartComplex)&xmlStartComplex,   (VisitEndComplex)&xmlEndComplex,
    (VisitSimpleElem)&xmlSimpleElem,
};
