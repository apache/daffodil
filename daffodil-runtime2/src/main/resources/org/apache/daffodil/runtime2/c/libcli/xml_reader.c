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

#include "xml_reader.h"
#include <assert.h>    // for assert
#include <errno.h>     // for errno
#include <inttypes.h>  // for strtoimax, strtoumax
#include <mxml.h>      // for mxmlWalkNext, mxmlGetElement, mxmlGetType, MXML_DESCEND, MXML_OPAQUE, mxmlDelete, mxmlGetOpaque, mxmlLoadFile, MXML_OPAQUE_CALLBACK
#include <stdbool.h>   // for bool, false, true
#include <stdint.h>    // for intmax_t, uintmax_t, int16_t, int32_t, int64_t, int8_t, uint16_t, uint32_t, uint64_t, uint8_t, INT16_MAX, INT16_MIN, INT32_MAX, INT32_MIN, INT64_MAX, INT64_MIN, INT8_MAX, INT8_MIN, UINT16_MAX, UINT32_MAX, UINT64_MAX, UINT8_MAX
#include <stdio.h>     // for NULL
#include <stdlib.h>    // for strtod, strtof
#include <string.h>    // for strcmp, strlen, strncmp
#include "errors.h"    // for Error, Error::(anonymous), ERR_STRTONUM_EMPTY, ERR_STRTONUM_NOT, ERR_XML_GONE, ERR_STRTOD_ERRNO, ERR_STRTOI_ERRNO, ERR_STRTONUM_RANGE, ERR_XML_MISMATCH, ERR_STRTOBOOL, ERR_XML_ERD, ERR_XML_INPUT, ERR_XML_LEFT, UNUSED

// Convert an XML element's text to a boolean with error checking

static bool
strtobool(const char *numptr, const Error **errorptr)
{
    // The lexical space of xs:boolean accepts true, false, 1, and 0
    bool value = false;

    // Check for any errors converting the string to a boolean
    if (strcmp(numptr, "true") == 0)
    {
        value = true;
    }
    else if (strcmp(numptr, "false") == 0)
    {
        value = false;
    }
    else if (strcmp(numptr, "1") == 0)
    {
        value = true;
    }
    else if (strcmp(numptr, "0") == 0)
    {
        value = false;
    }
    else
    {
        static Error error = {ERR_STRTOBOOL, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }

    return value;
}

// Convert an XML element's text to a double (call strtod with our own
// error checking)

static double
strtodnum(const char *numptr, const Error **errorptr)
{
    char *endptr = NULL;

    // Clear errno to detect error after calling strtod
    errno = 0;
    const double value = strtod(numptr, &endptr);

    // Check for any errors converting the string to a number
    if (errno != 0)
    {
        static Error error = {ERR_STRTOD_ERRNO, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }
    else if (endptr == numptr)
    {
        static Error error = {ERR_STRTONUM_EMPTY, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }
    else if (*endptr != '\0')
    {
        static Error error = {ERR_STRTONUM_NOT, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }

    return value;
}

// Convert an XML element's text to a float (call strtof with our own
// error checking)

static float
strtofnum(const char *numptr, const Error **errorptr)
{
    char *endptr = NULL;

    // Clear errno to detect error after calling strtof
    errno = 0;
    const float value = strtof(numptr, &endptr);

    // Check for any errors converting the string to a number
    if (errno != 0)
    {
        static Error error = {ERR_STRTOD_ERRNO, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }
    else if (endptr == numptr)
    {
        static Error error = {ERR_STRTONUM_EMPTY, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }
    else if (*endptr != '\0')
    {
        static Error error = {ERR_STRTONUM_NOT, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }

    return value;
}

// Convert an XML element's text to a signed integer (call strtoimax
// with our own error checking)

static intmax_t
strtonum(const char *numptr, intmax_t minval, intmax_t maxval, const Error **errorptr)
{
    char *endptr = NULL;
    assert(minval < maxval);

    // Clear errno to detect error after calling strtoimax
    errno = 0;
    const intmax_t value = strtoimax(numptr, &endptr, 10);

    // Check for any errors converting the string to a number
    if (errno != 0)
    {
        static Error error = {ERR_STRTOI_ERRNO, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }
    else if (endptr == numptr)
    {
        static Error error = {ERR_STRTONUM_EMPTY, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }
    else if (*endptr != '\0')
    {
        static Error error = {ERR_STRTONUM_NOT, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }
    else if (value < minval || value > maxval)
    {
        static Error error = {ERR_STRTONUM_RANGE, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }

    return value;
}

// Convert an XML element's text to an unsigned integer (call strtoumax
// with our own error checking)

static uintmax_t
strtounum(const char *numptr, uintmax_t maxval, const Error **errorptr)
{
    char *endptr = NULL;

    // Clear errno to detect error after calling strtoumax
    errno = 0;
    const uintmax_t value = strtoumax(numptr, &endptr, 10);

    // Check for any errors converting the string to a number
    if (errno != 0)
    {
        static Error error = {ERR_STRTOI_ERRNO, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }
    else if (endptr == numptr)
    {
        static Error error = {ERR_STRTONUM_EMPTY, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }
    else if (*endptr != '\0')
    {
        static Error error = {ERR_STRTONUM_NOT, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }
    else if (value > maxval)
    {
        static Error error = {ERR_STRTONUM_RANGE, {NULL}};
        error.s = numptr;
        *errorptr = &error;
    }

    return value;
}

// Read XML data from file before walking infoset

static const Error *
xmlStartDocument(XMLReader *reader)
{
    // Load the XML data into memory
    reader->xml = mxmlLoadFile(NULL, reader->stream, MXML_OPAQUE_CALLBACK);
    reader->node = reader->xml;
    if (!reader->node)
    {
        static Error error = {ERR_XML_INPUT, {NULL}};
        return &error;
    }

    // Consume the <?xml line if there is one
    const char *name = mxmlGetElement(reader->node);
    if (name && strncmp(name, "?xml", strlen("?xml")) == 0)
    {
        do
        {
            reader->node = mxmlWalkNext(reader->node, reader->xml, MXML_DESCEND);
        } while (mxmlGetType(reader->node) == MXML_OPAQUE);
        name = mxmlGetElement(reader->node);
    }

    // Consume a comment if there is one
    if (name && strncmp(name, "!--", strlen("!--")) == 0)
    {
        do
        {
            reader->node = mxmlWalkNext(reader->node, reader->xml, MXML_DESCEND);
        } while (mxmlGetType(reader->node) == MXML_OPAQUE);
    }

    static Error error = {ERR_XML_GONE, {NULL}};
    return reader->node ? NULL : &error;
}

// Delete XML data after walking infoset

static const Error *
xmlEndDocument(XMLReader *reader)
{
    // Consume any remaining newlines or whitespace
    while (mxmlGetType(reader->node) == MXML_OPAQUE)
    {
        reader->node = mxmlWalkNext(reader->node, reader->xml, MXML_DESCEND);
    }

    // Check whether we have consumed all of the XML data
    if (reader->node)
    {
        // This code path exits the program - no need to call mxmlDelete
        static Error error = {ERR_XML_LEFT, {NULL}};
        error.s = mxmlGetElement(reader->node);
        return &error;
    }

    // Free the storage allocated to hold the XML data
    mxmlDelete(reader->xml);
    reader->xml = NULL;
    reader->node = NULL;
    return NULL;
}

// Continue walking both XML data and infoset in lockstep

static const Error *
xmlStartComplex(XMLReader *reader, const InfosetBase *base)
{
    // Consume any newlines or whitespace before the element
    while (mxmlGetType(reader->node) == MXML_OPAQUE)
    {
        reader->node = mxmlWalkNext(reader->node, reader->xml, MXML_DESCEND);
    }

    // Get the element and consume it
    const char *name_from_xml = mxmlGetElement(reader->node);
    const char *name_from_erd = get_erd_name(base->erd);
    reader->node = mxmlWalkNext(reader->node, reader->xml, MXML_DESCEND);

    // Check whether we are walking both XML data and infoset in lockstep
    if (name_from_xml && name_from_erd)
    {
        static Error error = {ERR_XML_MISMATCH, {NULL}};
        error.s = name_from_erd;
        return strcmp(name_from_xml, name_from_erd) == 0 ? NULL : &error;
    }
    else
    {
        static Error error = {ERR_XML_GONE, {NULL}};
        return &error;
    }
}

// Consume XML data only on start events, not end events

static const Error *
xmlEndComplex(XMLReader *reader, const InfosetBase *base)
{
    UNUSED(reader); // because nothing to read
    UNUSED(base);   // because nothing to check
    return NULL;
}

// Read a boolean, 32-bit or 64-bit real number, or 8, 16, 32, or
// 64-bit signed or unsigned integer from XML data

static const Error *
xmlNumberElem(XMLReader *reader, const ERD *erd, void *number)
{
    // Consume any newlines or whitespace before the element
    while (mxmlGetType(reader->node) == MXML_OPAQUE)
    {
        reader->node = mxmlWalkNext(reader->node, reader->xml, MXML_DESCEND);
    }

    // Get the element and consume it
    const char *name_from_xml = mxmlGetElement(reader->node);
    const char *name_from_erd = get_erd_name(erd);
    const char *number_from_xml = mxmlGetOpaque(reader->node);
    reader->node = mxmlWalkNext(reader->node, reader->xml, MXML_DESCEND);

    // Check whether we are walking both XML data and infoset in lockstep
    if (name_from_xml && name_from_erd)
    {
        if (strcmp(name_from_xml, name_from_erd) == 0)
        {
            // Check for any errors getting the number
            const Error *error = NULL;

            // Handle varying bit lengths of both signed & unsigned numbers
            const enum TypeCode typeCode = erd->typeCode;
            switch (typeCode)
            {
            case PRIMITIVE_BOOLEAN:
                *(bool *)number = strtobool(number_from_xml, &error);
                return error;
            case PRIMITIVE_FLOAT:
                *(float *)number = strtofnum(number_from_xml, &error);
                return error;
            case PRIMITIVE_DOUBLE:
                *(double *)number = strtodnum(number_from_xml, &error);
                return error;
            case PRIMITIVE_INT16:
                *(int16_t *)number = (int16_t)strtonum(number_from_xml, INT16_MIN, INT16_MAX, &error);
                return error;
            case PRIMITIVE_INT32:
                *(int32_t *)number = (int32_t)strtonum(number_from_xml, INT32_MIN, INT32_MAX, &error);
                return error;
            case PRIMITIVE_INT64:
                *(int64_t *)number = (int64_t)strtonum(number_from_xml, INT64_MIN, INT64_MAX, &error);
                return error;
            case PRIMITIVE_INT8:
                *(int8_t *)number = (int8_t)strtonum(number_from_xml, INT8_MIN, INT8_MAX, &error);
                return error;
            case PRIMITIVE_UINT16:
                *(uint16_t *)number = (uint16_t)strtounum(number_from_xml, UINT16_MAX, &error);
                return error;
            case PRIMITIVE_UINT32:
                *(uint32_t *)number = (uint32_t)strtounum(number_from_xml, UINT32_MAX, &error);
                return error;
            case PRIMITIVE_UINT64:
                *(uint64_t *)number = (uint64_t)strtounum(number_from_xml, UINT64_MAX, &error);
                return error;
            case PRIMITIVE_UINT8:
                *(uint8_t *)number = (uint8_t)strtounum(number_from_xml, UINT8_MAX, &error);
                return error;
            default:
            {
                static Error error_erd = {ERR_XML_ERD, {NULL}};
                error_erd.d64 = typeCode;
                return &error_erd;
            }
            }
        }
        else
        {
            static Error error = {ERR_XML_MISMATCH, {NULL}};
            error.s = name_from_erd;
            return &error;
        }
    }
    else
    {
        static Error error = {ERR_XML_GONE, {NULL}};
        return &error;
    }
}

// Initialize a struct with our visitor event handler methods

const VisitEventHandler xmlReaderMethods = {
    (VisitStartDocument)&xmlStartDocument, (VisitEndDocument)&xmlEndDocument,
    (VisitStartComplex)&xmlStartComplex,   (VisitEndComplex)&xmlEndComplex,
    (VisitNumberElem)&xmlNumberElem,
};
