#
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Step 0: You will need to install the Mini-XML library and xmldiff.
# Here's how to install both packages on Ubuntu 20.04 (first-time
# setup only):

# $ sudo apt install libmxml-dev xmldiff

# Step 1: Copy your test data files here and either rename them to
# parse.dat and unparse.xml or set PARSE_DAT and UNPARSE_XML.

# $ cp ../ex_nums_parse.dat parse.dat
# $ cp ../ex_nums_unparse_runtime2.xml unparse.xml

PARSE_DAT = parse.dat
UNPARSE_XML = unparse.xml

# Step 2: Compile the C source files into an executable program which
# can run the parse and unparse checks (e.g., .dat <-> .xml).

# $ make

PROGRAM = ./daffodil
HEADERS = libcli/*.h libruntime/*.h
SOURCES = libcli/*.c libruntime/*.c
INCLUDES = -Ilibcli -Ilibruntime
CFLAGS = -g -Wall -Wextra -pedantic -std=gnu99
LIBS = -lmxml

$(PROGRAM): $(HEADERS) $(SOURCES)
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o $(PROGRAM)

# Step 3: Run the executable on the test data files and check that the
# new temp data files match the original test data files.

# $ make check

check: parse-check unparse-check

parse-check: $(PROGRAM)
	$(PROGRAM) -o temp_$(UNPARSE_XML) parse $(PARSE_DAT)
	xmldiff $(UNPARSE_XML) temp_$(UNPARSE_XML)

unparse-check: $(PROGRAM)
	$(PROGRAM) -o temp_$(PARSE_DAT) unparse $(UNPARSE_XML)
	diff $(PARSE_DAT) temp_$(PARSE_DAT)

# Step 4 (optional): Remove the executable and temp data files.

# $ make clean

clean:
	rm -f $(PROGRAM) temp_$(PARSE_DAT) temp_$(UNPARSE_XML)

.PHONY: check parse-check unparse-check clean
