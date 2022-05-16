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

# Step 0: Install the Mini-XML library and xmldiff (first time setup
# only).  Here's the command to install both packages on Ubuntu.

# $ sudo apt install libmxml-dev xmldiff

# Step 1: Compile the C source files into an executable program which
# can run the parse and unparse checks (e.g., .dat <-> .xml).  Just
# run make with no arguments unless you want to override CC or CFLAGS.

# $ make

PROGRAM = ./daffodil
HEADERS = libcli/*.h libruntime/*.h
SOURCES = libcli/*.c libruntime/*.c
INCLUDES = -Ilibcli -Ilibruntime
CFLAGS = -g -Wall -Wextra -Wpedantic -std=gnu11
LIBS = -lmxml

$(PROGRAM): $(HEADERS) $(SOURCES)
	$(CC) $(CFLAGS) $(INCLUDES) $(SOURCES) $(LIBS) -o $(PROGRAM)

# Step 1.5: Run the tests if you are a developer making changes to the
# C code, otherwise you can skip this step.  On Ubuntu 20.04, you will
# need to manually build and install a C test framework library called
# Criterion (https://github.com/Snaipe/Criterion); note building
# Criterion requires meson ninja-build libffi-dev libgit2-dev.

TPROGRAM = ./ctests
EXCLUDES = %/daffodil_main.c %/generated_code.c
LSOURCES = $(filter-out $(EXCLUDES),$(wildcard $(SOURCES)))
TSOURCES = tests/*.c
TINCLUDES = $(INCLUDES) -I/usr/local/include
TLIBS = $(LIBS) -lcriterion

$(TPROGRAM): $(HEADERS) $(LSOURCES) $(TSOURCES)
	$(CC) $(CFLAGS) $(TINCLUDES) $(LSOURCES) $(TSOURCES) $(TLIBS) -o $(TPROGRAM)

test: $(TPROGRAM)
	$(TPROGRAM) #--verbose --debug=gdb --filter=bits/be_signed_integers

# Step 2: Copy your test files here and rename them to test.dat and
# test.dat.xml or else edit TEST_DAT and TEST_DAT_XML below.

# $ cp ../ex_nums.dat test.dat
# $ cp ../ex_nums.dat.xml.runtime2 test.dat.xml

TEST_DAT = test.dat
TEST_DAT_XML = test.dat.xml

# Step 3: Run the executable on the test files and check that the new
# scratch files match the original test files.

# $ make check

check: parse-check unparse-check

parse-check: $(PROGRAM)
	$(PROGRAM) -o $(TEST_DAT_XML).tmp parse $(TEST_DAT)
	xmldiff $(TEST_DAT_XML) $(TEST_DAT_XML).tmp

unparse-check: $(PROGRAM)
	$(PROGRAM) -o $(TEST_DAT).tmp unparse $(TEST_DAT_XML)
	diff $(TEST_DAT) $(TEST_DAT).tmp

# Step 4: Remove the executable and scratch files (optional).

# $ make clean

clean:
	rm -f $(PROGRAM) $(TPROGRAM) $(TEST_DAT).tmp $(TEST_DAT_XML).tmp

# Maintainer only: Format C source files or check includes.

FMT = clang-format -i
IWYU = iwyu -Xiwyu --max_line_length=999 -Xiwyu --update_comments

format:
	$(FMT) $(HEADERS) $(SOURCES) $(TSOURCES)

iwyu:
	-for f in $(SOURCES) $(TSOURCES); do $(IWYU) $(CFLAGS) $(INCLUDES) $$f; done

.PHONY: test check parse-check unparse-check clean format iwyu
