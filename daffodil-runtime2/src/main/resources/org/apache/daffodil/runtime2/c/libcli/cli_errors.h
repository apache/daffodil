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

#ifndef CLI_ERRORS_H
#define CLI_ERRORS_H

// clang-format off
#include "errors.h"  // for ERR_ZZZ
// clang-format on

// CliCode - identifiers of libcli errors

enum CliCode
{
    CLI_FILE_CLOSE = ERR_ZZZ,
    CLI_FILE_OPEN,
    CLI_HELP_USAGE,
    CLI_HEXBINARY_LENGTH,
    CLI_HEXBINARY_PARSE,
    CLI_HEXBINARY_SIZE,
    CLI_INVALID_COMMAND,
    CLI_INVALID_INFOSET,
    CLI_INVALID_OPTION,
    CLI_MISSING_COMMAND,
    CLI_MISSING_VALUE,
    CLI_PROGRAM_ERROR,
    CLI_PROGRAM_VERSION,
    CLI_STACK_EMPTY,
    CLI_STACK_OVERFLOW,
    CLI_STACK_UNDERFLOW,
    CLI_STRTOBOOL,
    CLI_STRTOD_ERRNO,
    CLI_STRTOI_ERRNO,
    CLI_STRTONUM_EMPTY,
    CLI_STRTONUM_NOT,
    CLI_STRTONUM_RANGE,
    CLI_UNEXPECTED_ARGUMENT,
    CLI_XML_DECL,
    CLI_XML_ELEMENT,
    CLI_XML_ERD,
    CLI_XML_GONE,
    CLI_XML_INPUT,
    CLI_XML_LEFT,
    CLI_XML_MISMATCH,
    CLI_XML_WRITE,
    CLI_ZZZ,
};

// CliLimits - limits on how many elements static arrays can hold

enum CliLimits
{
    LIMIT_XML_NESTING = 100, // limits how deep infoset elements can nest
};

#endif // CLI_ERRORS_H
