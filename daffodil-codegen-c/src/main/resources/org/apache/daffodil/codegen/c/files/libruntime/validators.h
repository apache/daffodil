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

#ifndef VALIDATORS_H
#define VALIDATORS_H

// auto-maintained by iwyu
// clang-format off
#include <stdbool.h>  // for bool
#include <stddef.h>   // for size_t
#include <stdint.h>   // for int64_t
#include "infoset.h"  // for ParserOrUnparserState
// clang-format on

// Validate element's array count is within its array bounds

extern void validate_array_bounds(const char *name, size_t count, size_t minOccurs, size_t maxOccurs,
                                  ParserOrUnparserState *pu);

// Validate element's value is same as its fixed attribute

extern void validate_fixed_attribute(bool same, const char *element, ParserOrUnparserState *pu);

// Validate element's value matches a floating point enumeration

extern void validate_floatpt_enumeration(double number, size_t num_enums, double enums[], const char *element,
                                         ParserOrUnparserState *pu);

// Validate element's value matches a hexBinary enumeration

extern void validate_hexbinary_enumeration(const HexBinary *hexBinary, size_t num_enums, HexBinary enums[],
                                           const char *element, ParserOrUnparserState *pu);

// Validate element's value matches an integer enumeration

extern void validate_integer_enumeration(int64_t number, size_t num_enums, int64_t enums[],
                                         const char *element, ParserOrUnparserState *pu);

// Validate element's value fits within its schema's allowed range

extern void validate_schema_range(bool within_range, const char *element, ParserOrUnparserState *pu);

#endif // VALIDATORS_H
