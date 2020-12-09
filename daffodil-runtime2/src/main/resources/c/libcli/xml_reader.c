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
#include <errno.h>    // for errno
#include <inttypes.h> // for strtoimax
#include <mxml.h>     // for mxmlWalkNext, mxmlGetType, mxmlGetElement, ...
#include <stdint.h>   // for intmax_t, int32_t, INT32_MAX, INT32_MIN
#include <string.h>   // for strcmp, strlen, strncmp

// Convert an XML element's text to an integer (BSD function not
// widely available, so roll our own function based on strtoimax)

static intmax_t
strtonum(const char *numptr, intmax_t minval, intmax_t maxval,
         const char **errstrp)
{
    char *endptr = NULL;

    // Clear errno to detect failure after calling strtoimax
    errno = 0;
    const intmax_t value = strtoimax(numptr, &endptr, 10);

    // Report any issues converting the string to a number
    if (errno != 0)
    {
        *errstrp = "Error converting XML data to integer";
    }
    else if (endptr == numptr)
    {
        *errstrp = "Found no number in XML data";
    }
    else if (*endptr != '\0')
    {
        *errstrp = "Found non-number characters in XML data";
    }
    else if (value < minval || value > maxval || maxval < minval)
    {
        *errstrp = "Number in XML data out of range";
    }
    else
    {
        *errstrp = NULL;
    }

    return value;
}

// Read XML data from file before walking infoset

static const char *
xmlStartDocument(XMLReader *reader)
{
    // Load the XML data into memory
    reader->xml = mxmlLoadFile(NULL, reader->stream, MXML_OPAQUE_CALLBACK);
    reader->node = reader->xml;
    if (!reader->node)
    {
        return "Unable to read XML data from input file";
    }

    // Consume the <?xml line if there is one
    const char *name = mxmlGetElement(reader->node);
    if (name && strncmp(name, "?xml", strlen("?xml")) == 0)
    {
        do
        {
            reader->node =
                mxmlWalkNext(reader->node, reader->xml, MXML_DESCEND);
        } while (mxmlGetType(reader->node) == MXML_OPAQUE);
        name = mxmlGetElement(reader->node);
    }

    // Consume a comment if there is one
    if (name && strncmp(name, "!--", strlen("!--")) == 0)
    {
        do
        {
            reader->node =
                mxmlWalkNext(reader->node, reader->xml, MXML_DESCEND);
        } while (mxmlGetType(reader->node) == MXML_OPAQUE);
    }

    return reader->node ? NULL : "Ran out of XML data";
}

// Delete XML data after walking infoset

static const char *
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
        return "Did not consume all of the XML data";
    }

    // Free the storage allocated to hold the XML data
    mxmlDelete(reader->xml);
    reader->xml = NULL;
    reader->node = NULL;
    return NULL;
}

// Continue walking both XML data and infoset in lockstep

static const char *
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
        return strcmp(name_from_xml, name_from_erd) == 0
                   ? NULL
                   : "Found mismatch between XML data and infoset";
    }
    else
    {
        return "Ran out of XML data";
    }
}

// Consume XML data only on start events, not end events

static const char *
xmlEndComplex(XMLReader *reader, const InfosetBase *base)
{
    (void)reader;
    (void)base;
    return NULL;
}

// Read 8, 16, 32, or 64-bit signed/unsigned integer number from XML data

static const char *
xmlIntegerElem(XMLReader *reader, const ERD *erd, void *intLocation)
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
            // Check for any errors getting the integer number
            const char *errstr = NULL;

            // Need to handle varying bit lengths and signedness
            const enum TypeCode typeCode = erd->typeCode;
            switch (typeCode)
            {
            case PRIMITIVE_UINT64:
                *(uint64_t *)intLocation =
                    (uint64_t)strtonum(number_from_xml, 0, UINT64_MAX, &errstr);
                break;
            case PRIMITIVE_UINT32:
                *(uint32_t *)intLocation =
                    (uint32_t)strtonum(number_from_xml, 0, UINT32_MAX, &errstr);
                break;
            case PRIMITIVE_UINT16:
                *(uint16_t *)intLocation =
                    (uint16_t)strtonum(number_from_xml, 0, UINT16_MAX, &errstr);
                break;
            case PRIMITIVE_UINT8:
                *(uint8_t *)intLocation =
                    (uint8_t)strtonum(number_from_xml, 0, UINT8_MAX, &errstr);
                break;
            case PRIMITIVE_INT64:
                *(int64_t *)intLocation = (int64_t)strtonum(
                    number_from_xml, INT64_MIN, INT64_MAX, &errstr);
                break;
            case PRIMITIVE_INT32:
                *(int32_t *)intLocation = (int32_t)strtonum(
                    number_from_xml, INT32_MIN, INT32_MAX, &errstr);
                break;
            case PRIMITIVE_INT16:
                *(int16_t *)intLocation = (int16_t)strtonum(
                    number_from_xml, INT16_MIN, INT16_MAX, &errstr);
                break;
            case PRIMITIVE_INT8:
                *(int8_t *)intLocation = (int8_t)strtonum(
                    number_from_xml, INT8_MIN, INT8_MAX, &errstr);
                break;
            default:
                errstr = "Unexpected ERD typeCode while reading integer from XML data";
                break;
            }

            return errstr;
        }
        else
        {
            return "Found mismatch between XML data and infoset";
        }
    }
    else
    {
        return "Ran out of XML data";
    }
}

// Initialize a struct with our visitor event handler methods

const VisitEventHandler xmlReaderMethods = {
    (VisitStartDocument)&xmlStartDocument, (VisitEndDocument)&xmlEndDocument,
    (VisitStartComplex)&xmlStartComplex,   (VisitEndComplex)&xmlEndComplex,
    (VisitIntegerElem)&xmlIntegerElem,
};
