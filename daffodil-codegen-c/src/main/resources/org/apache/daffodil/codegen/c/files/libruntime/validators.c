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

// auto-maintained by iwyu
// clang-format off
#include "validators.h"
#include <stdbool.h>  // for bool, false, true
#include <string.h>   // for memcmp
#include "errors.h"   // for add_diagnostic, get_diagnostics, Error, Diagnostics, ERR_RESTR_ENUM, ERR_ARRAY_BOUNDS, ERR_RESTR_FIXED, ERR_RESTR_RANGE, Error::(anonymous)
// clang-format on

// Validate element's array count is within its array bounds

void
validate_array_bounds(const char *name, size_t count, size_t minOccurs, size_t maxOccurs,
                      ParserOrUnparserState *pu)
{
    if (count < minOccurs || count > maxOccurs)
    {
        // Array count is not within bounds, so report error
        static Error error = {ERR_ARRAY_BOUNDS, {0}};
        error.arg.s = name;
        pu->error = &error;
    }
}

// Validate element's value is same as its fixed attribute

void
validate_fixed_attribute(bool same, const char *element, ParserOrUnparserState *pu)
{
    if (!same)
    {
        // Element is not same as its fixed attribute, so diagnose problem
        Diagnostics *diagnostics = get_diagnostics();
        const Error error = {ERR_RESTR_FIXED, {.s = element}};

        add_diagnostic(diagnostics, &error);
        pu->diagnostics = diagnostics;
    }
}

// Validate element's value matches a floating point enumeration

void
validate_floatpt_enumeration(double number, size_t num_enums, double enums[], const char *element,
                             ParserOrUnparserState *pu)
{
    bool match_found = false;
    for (size_t i = 0; !match_found && i < num_enums; i++)
    {
        if (number == enums[i])
        {
            match_found = true;
        }
    }
    if (!match_found)
    {
        // Number does not match any enumeration, so diagnose problem
        Diagnostics *diagnostics = get_diagnostics();
        const Error error = {ERR_RESTR_ENUM, {.s = element}};

        add_diagnostic(diagnostics, &error);
        pu->diagnostics = diagnostics;
    }
}

// Validate element's value matches a hexBinary enumeration
// (matches the actual data bytes, not the hex digit characters)

void
validate_hexbinary_enumeration(const HexBinary *hexBinary, size_t num_enums, HexBinary enums[],
                               const char *element, ParserOrUnparserState *pu)
{
    bool match_found = false;
    for (size_t i = 0; !match_found && i < num_enums; i++)
    {
        bool same_lengths = hexBinary->lengthInBytes == enums[i].lengthInBytes;
        if (same_lengths && memcmp(hexBinary->array, enums[i].array, hexBinary->lengthInBytes) == 0)
        {
            match_found = true;
        }
    }
    if (!match_found)
    {
        // HexBinary does not match any enumeration value, so report error
        Diagnostics *diagnostics = get_diagnostics();
        const Error error = {ERR_RESTR_ENUM, {.s = element}};

        add_diagnostic(diagnostics, &error);
        pu->diagnostics = diagnostics;
    }
}

// Validate element's value matches an integer enumeration

void
validate_integer_enumeration(int64_t number, size_t num_enums, int64_t enums[], const char *element,
                             ParserOrUnparserState *pu)
{
    bool match_found = false;
    for (size_t i = 0; !match_found && i < num_enums; i++)
    {
        if (number == enums[i])
        {
            match_found = true;
        }
    }
    if (!match_found)
    {
        // Number does not match any enumeration value, so report error
        Diagnostics *diagnostics = get_diagnostics();
        const Error error = {ERR_RESTR_ENUM, {.s = element}};

        add_diagnostic(diagnostics, &error);
        pu->diagnostics = diagnostics;
    }
}

// Validate element's value fits within its schema's allowed range

void
validate_schema_range(bool within_range, const char *element, ParserOrUnparserState *pu)
{
    // The boolean expression that determines if a field is in range
    // is evaluated in generated code and the result passed here
    if (!within_range)
    {
        Diagnostics *diagnostics = get_diagnostics();
        const Error error = {ERR_RESTR_RANGE, {.s = element}};

        add_diagnostic(diagnostics, &error);
        pu->diagnostics = diagnostics;
    }
}
