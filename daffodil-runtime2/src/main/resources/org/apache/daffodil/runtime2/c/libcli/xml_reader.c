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
#include "xml_reader.h"
#include <assert.h>      // for assert
#include <errno.h>       // for errno
#include <inttypes.h>    // for strtoimax, strtoumax
#include <mxml.h>        // for mxmlWalkNext, mxmlGetElement, mxmlGetType, MXML_DESCEND, MXML_OPAQUE, mxmlDelete, mxmlGetOpaque, mxmlLoadFile, MXML_OPAQUE_CALLBACK
#include <stdbool.h>     // for bool, false, true
#include <stdint.h>      // for int64_t, intmax_t, uint8_t, uintmax_t, int16_t, int32_t, int8_t, uint16_t, uint32_t, uint64_t, INT16_MAX, INT16_MIN, INT32_MAX, INT32_MIN, INT64_MAX, INT64_MIN, INT8_MAX, INT8_MIN, UINT16_MAX, UINT32_MAX, UINT64_MAX, UINT8_MAX
#include <stdlib.h>      // for free, malloc, strtod, strtof
#include <string.h>      // for strcmp, strlen, strncmp, memset
#include "cli_errors.h"  // for CLI_STRTONUM_EMPTY, CLI_STRTONUM_NOT, CLI_XML_GONE, CLI_STRTOD_ERRNO, CLI_STRTOI_ERRNO, CLI_STRTONUM_RANGE, CLI_XML_MISMATCH, CLI_HEXBINARY_LENGTH, CLI_HEXBINARY_PARSE, CLI_HEXBINARY_SIZE, CLI_STRTOBOOL, CLI_XML_ERD, CLI_XML_INPUT, CLI_XML_LEFT
#include "errors.h"      // for Error, Error::(anonymous), ERR_HEXBINARY_ALLOC, UNUSED
// clang-format on

// Convert an XML element's text to a boolean with error checking

static const Error *
strtobool(const char *text, bool *valueptr)
{
    // The lexical space of xs:boolean accepts true, false, 1, and 0
    bool value = false;

    // Check for any errors converting the string to a boolean
    if (strcmp(text, "true") == 0)
    {
        value = true;
    }
    else if (strcmp(text, "false") == 0)
    {
        value = false;
    }
    else if (strcmp(text, "1") == 0)
    {
        value = true;
    }
    else if (strcmp(text, "0") == 0)
    {
        value = false;
    }
    else
    {
        static Error error = {CLI_STRTOBOOL, {0}};
        error.arg.s = text;
        return &error;
    }

    *valueptr = value;
    return NULL;
}

// Convert an XML element's text to a double (call strtod with our own
// error checking)

static const Error *
strtodnum(const char *text, double *valueptr)
{
    // Should point to text's end after conversion
    char *endptr = NULL;

    // Clear errno to detect error after calling strtod
    errno = 0;
    const double value = strtod(text, &endptr);

    // Check for any errors converting the text to a number
    if (errno != 0)
    {
        static Error error = {CLI_STRTOD_ERRNO, {0}};
        error.arg.s = text;
        return &error;
    }
    else if (endptr == text)
    {
        static Error error = {CLI_STRTONUM_EMPTY, {0}};
        error.arg.s = text;
        return &error;
    }
    else if (*endptr != '\0')
    {
        static Error error = {CLI_STRTONUM_NOT, {0}};
        error.arg.s = text;
        return &error;
    }

    *valueptr = value;
    return NULL;
}

// Convert an XML element's text to a float (call strtof with our own
// error checking)

static const Error *
strtofnum(const char *text, float *valueptr)
{
    // Should point to text's end after conversion
    char *endptr = NULL;

    // Clear errno to detect error after calling strtof
    errno = 0;
    const float value = strtof(text, &endptr);

    // Check for any errors converting the text to a number
    if (errno != 0)
    {
        static Error error = {CLI_STRTOD_ERRNO, {0}};
        error.arg.s = text;
        return &error;
    }
    else if (endptr == text)
    {
        static Error error = {CLI_STRTONUM_EMPTY, {0}};
        error.arg.s = text;
        return &error;
    }
    else if (*endptr != '\0')
    {
        static Error error = {CLI_STRTONUM_NOT, {0}};
        error.arg.s = text;
        return &error;
    }

    *valueptr = value;
    return NULL;
}

// Convert an XML element's text to a signed integer (call strtoimax
// with our own error checking)

static const Error *
strtoinum(const char *text, intmax_t minval, intmax_t maxval, intmax_t *valueptr)
{
    // Should point to text's end after conversion
    char *endptr = NULL;
    assert(minval < maxval);

    // Clear errno to detect error after calling strtoimax
    errno = 0;
    const intmax_t value = strtoimax(text, &endptr, 10);

    // Check for any errors converting the text to a number
    if (errno != 0)
    {
        static Error error = {CLI_STRTOI_ERRNO, {0}};
        error.arg.s = text;
        return &error;
    }
    else if (endptr == text)
    {
        static Error error = {CLI_STRTONUM_EMPTY, {0}};
        error.arg.s = text;
        return &error;
    }
    else if (*endptr != '\0')
    {
        static Error error = {CLI_STRTONUM_NOT, {0}};
        error.arg.s = text;
        return &error;
    }
    else if (value < minval || value > maxval)
    {
        static Error error = {CLI_STRTONUM_RANGE, {0}};
        error.arg.s = text;
        return &error;
    }

    *valueptr = value;
    return NULL;
}

// Convert an XML element's text to an unsigned integer (call strtoumax
// with our own error checking)

static const Error *
strtounum(const char *text, uintmax_t maxval, uintmax_t *valueptr)
{
    // Should point to text's end after conversion
    char *endptr = NULL;

    // Clear errno to detect error after calling strtoumax
    errno = 0;
    const uintmax_t value = strtoumax(text, &endptr, 10);

    // Check for any errors converting the text to a number
    if (errno != 0)
    {
        static Error error = {CLI_STRTOI_ERRNO, {0}};
        error.arg.s = text;
        return &error;
    }
    else if (endptr == text)
    {
        static Error error = {CLI_STRTONUM_EMPTY, {0}};
        error.arg.s = text;
        return &error;
    }
    else if (*endptr != '\0')
    {
        static Error error = {CLI_STRTONUM_NOT, {0}};
        error.arg.s = text;
        return &error;
    }
    else if (value > maxval)
    {
        static Error error = {CLI_STRTONUM_RANGE, {0}};
        error.arg.s = text;
        return &error;
    }

    *valueptr = value;
    return NULL;
}

// Store an XML element's text (a string of hexadecimal characters,
// two nibbles per byte) into a byte array.  Allocate memory for byte
// array if needed.  Return error if text does not fit into byte
// array or does not contain valid hexadecimal characters.

static const Error *
strtohexbinary(const char *text, HexBinary *hexBinary)
{
    // Check whether text has even number of hexadecimal characters
    size_t numNibbles = text ? strlen(text) : 0;
    size_t numBytes = numNibbles / 2;
    if ((numNibbles % 2) != 0)
    {
        static Error error = {CLI_HEXBINARY_LENGTH, {0}};
        error.arg.d64 = (int64_t)numNibbles;
        return &error;
    }

    // Allocate memory for byte array if needed
    if (hexBinary->dynamic && hexBinary->lengthInBytes < numBytes)
    {
        free(hexBinary->array);
        hexBinary->array = malloc(numBytes);
        hexBinary->lengthInBytes = numBytes;
        if (hexBinary->array == NULL)
        {
            static Error error = {ERR_HEXBINARY_ALLOC, {0}};
            error.arg.d64 = (int64_t)numBytes;
            return &error;
        }
    }

    // Check whether data fits into byte array
    if (hexBinary->lengthInBytes < numBytes)
    {
        static Error error = {CLI_HEXBINARY_SIZE, {0}};
        error.arg.d64 = (int64_t)hexBinary->lengthInBytes;
        return &error;
    }

    // Store hexadecimal characters into byte array
    if (hexBinary->array)
    {
        memset(hexBinary->array, 0, hexBinary->lengthInBytes);
        for (size_t i = 0; i < numNibbles; i++)
        {
            char    c = text[i];
            uint8_t value = 0;

            // Check whether c is valid hexadecimal character
            if (c >= '0' && c <= '9')
            {
                value = (c - '0');
            }
            else if (c >= 'A' && c <= 'F')
            {
                value = (c - 'A') + 10;
            }
            else if (c >= 'a' && c <= 'f')
            {
                value = (c - 'a') + 10;
            }
            else
            {
                static Error error = {CLI_HEXBINARY_PARSE, {0}};
                error.arg.c = c;
                return &error;
            }

            // Shift high nibble, add low nibble on next iteration
            value <<= (((i + 1) % 2) * 4);
            hexBinary->array[i / 2] += value;
        }
    }

    return NULL;
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
        static Error error = {CLI_XML_INPUT, {0}};
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

    static Error error = {CLI_XML_GONE, {0}};
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
        static Error error = {CLI_XML_LEFT, {0}};
        error.arg.s = mxmlGetElement(reader->node);
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
        static Error error = {CLI_XML_MISMATCH, {0}};
        error.arg.s = name_from_erd;
        return strcmp(name_from_xml, name_from_erd) == 0 ? NULL : &error;
    }
    else
    {
        static Error error = {CLI_XML_GONE, {0}};
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

// Read a boolean, 32-bit or 64-bit real number, hexBinary, or
// 8, 16, 32, or 64-bit signed or unsigned integer from XML data

static const Error *
xmlSimpleElem(XMLReader *reader, const ERD *erd, void *valueptr)
{
    // Consume any newlines or whitespace before the element
    while (mxmlGetType(reader->node) == MXML_OPAQUE)
    {
        reader->node = mxmlWalkNext(reader->node, reader->xml, MXML_DESCEND);
    }

    // Get the element and consume it
    const char *name_from_xml = mxmlGetElement(reader->node);
    const char *name_from_erd = get_erd_name(erd);
    const char *text = mxmlGetOpaque(reader->node);
    reader->node = mxmlWalkNext(reader->node, reader->xml, MXML_DESCEND);

    // Check whether we are walking both XML data and infoset in lockstep
    if (name_from_xml && name_from_erd)
    {
        if (strcmp(name_from_xml, name_from_erd) == 0)
        {
            // Check for any errors calling strtonum or strtounum
            const Error *error = NULL;
            intmax_t     num = 0;
            uintmax_t    unum = 0;

            // Handle various types of values
            const enum TypeCode typeCode = erd->typeCode;
            switch (typeCode)
            {
            case PRIMITIVE_BOOLEAN:
                return strtobool(text, (bool *)valueptr);
            case PRIMITIVE_FLOAT:
                return strtofnum(text, (float *)valueptr);
            case PRIMITIVE_DOUBLE:
                return strtodnum(text, (double *)valueptr);
            case PRIMITIVE_HEXBINARY:
                return strtohexbinary(text, (HexBinary *)valueptr);
            case PRIMITIVE_INT16:
                error = strtoinum(text, INT16_MIN, INT16_MAX, &num);
                *(int16_t *)valueptr = (int16_t)num;
                return error;
            case PRIMITIVE_INT32:
                error = strtoinum(text, INT32_MIN, INT32_MAX, &num);
                *(int32_t *)valueptr = (int32_t)num;
                return error;
            case PRIMITIVE_INT64:
                error = strtoinum(text, INT64_MIN, INT64_MAX, &num);
                *(int64_t *)valueptr = (int64_t)num;
                return error;
            case PRIMITIVE_INT8:
                error = strtoinum(text, INT8_MIN, INT8_MAX, &num);
                *(int8_t *)valueptr = (int8_t)num;
                return error;
            case PRIMITIVE_UINT16:
                error = strtounum(text, UINT16_MAX, &unum);
                *(uint16_t *)valueptr = (uint16_t)unum;
                return error;
            case PRIMITIVE_UINT32:
                error = strtounum(text, UINT32_MAX, &unum);
                *(uint32_t *)valueptr = (uint32_t)unum;
                return error;
            case PRIMITIVE_UINT64:
                error = strtounum(text, UINT64_MAX, &unum);
                *(uint64_t *)valueptr = (uint64_t)unum;
                return error;
            case PRIMITIVE_UINT8:
                error = strtounum(text, UINT8_MAX, &unum);
                *(uint8_t *)valueptr = (uint8_t)unum;
                return error;
            default:
            {
                static Error error_erd = {CLI_XML_ERD, {0}};
                error_erd.arg.d64 = typeCode;
                return &error_erd;
            }
            }
        }
        else
        {
            static Error error = {CLI_XML_MISMATCH, {0}};
            error.arg.s = name_from_erd;
            return &error;
        }
    }
    else
    {
        static Error error = {CLI_XML_GONE, {0}};
        return &error;
    }
}

// Initialize a struct with our visitor event handler methods

const VisitEventHandler xmlReaderMethods = {
    (VisitStartDocument)&xmlStartDocument, (VisitEndDocument)&xmlEndDocument,
    (VisitStartComplex)&xmlStartComplex,   (VisitEndComplex)&xmlEndComplex,
    (VisitSimpleElem)&xmlSimpleElem,
};
