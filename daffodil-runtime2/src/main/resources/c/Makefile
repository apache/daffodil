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

# Here's how to compile the C sources into a program for running
# parse/unparse tests (.dat <-> .xml)

PROGRAM = ./daffodil
HEADERS = libcli/*.h libruntime/*.h
SOURCES = libcli/*.c libruntime/*.c
INCLUDES = -I libcli -I libruntime
CFLAGS = -g -Wall -Wextra
LIBS = -lmxml

$(PROGRAM): $(HEADERS) $(SOURCES)
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o $(PROGRAM)

# Here's how to run parse/unparse tests (.dat <-> .xml, although you
# will need to create the .dat and .xml files first)

PARSE_DAT = parse.dat
UNPARSE_XML = unparse.xml

clean:
	rm -f $(PROGRAM) test_$(PARSE_DAT) test_$(UNPARSE_XML)

tests: parse-test unparse-test

parse-test: $(PROGRAM)
	$(PROGRAM) parse $(PARSE_DAT) -o test_$(UNPARSE_XML)
	xmldiff $(UNPARSE_XML) test_$(UNPARSE_XML)

unparse-test: $(PROGRAM)
	$(PROGRAM) unparse $(UNPARSE_XML) -o test_$(PARSE_DAT)
	diff $(PARSE_DAT) test_$(PARSE_DAT)

# You will need the Mini-XML library and xmldiff - here's how to
# install both in Ubuntu 20.04

deps:
	sudo apt install libmxml-dev xmldiff

.PHONY: deps tests parse-test unparse-test
