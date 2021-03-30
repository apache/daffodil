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

#ifndef DAFFODIL_ARGP_H
#define DAFFODIL_ARGP_H

// Parse our "daffodil" command line interface

extern int parse_daffodil_cli(int argc, char **argv);

// Get our "daffodil" CLI options

extern struct daffodil_cli
{
    enum daffodil_subcommand
    {
        DAFFODIL_NONE,
        DAFFODIL_PARSE,
        DAFFODIL_UNPARSE
    } subcommand;
    int verbosity;
} daffodil_cli;

// Get our "daffodil parse" CLI options

extern struct daffodil_parse_cli
{
    const char *infoset_converter;
    const char *infile;
    const char *outfile;
} daffodil_parse;

// Get our "daffodil unparse" CLI options

extern struct daffodil_unparse_cli
{
    const char *infoset_converter;
    const char *infile;
    const char *outfile;
} daffodil_unparse;

#endif // DAFFODIL_ARGP_H
