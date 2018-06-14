<!--
  Licensed to the Apache Software Foundation (ASF) under one or more
  contributor license agreements.  See the NOTICE file distributed with
  this work for additional information regarding copyright ownership.
  The ASF licenses this file to You under the Apache License, Version 2.0
  (the "License"); you may not use this file except in compliance with
  the License.  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-->

# Apache Daffodil (incubating)

## Introduction

Apache Daffodil (incubating) is the open source implementation of the [Data Format Description Language (DFDL)](http://www.ogf.org/dfdl), a specification created by the [Open Grid Forum](http://www.ogf.org). DFDL is capable of describing many data formats, including textual and binary, commercial record-oriented, scientific and numeric, modern and legacy, and many industry standards. It leverages XML technology and concepts, using a subset of W3C XML schema type system and annotations to describe such data. Daffodil uses this description to parse data into an infoset represented as XML or JSON, easily capable of ingestion, validation, and transformation.

For more information about Daffodil, see https://daffodil.apache.org/.

## Build Requirements

* JDK 1.8
* SBT 0.13.8 or higher

## Getting Started

To build Daffodil:

```bash
$ sbt clean package
```
To run all Daffodil tests:

```bash
# run all unit tests
$ sbt test 
# run all integration tests
$ sbt it:test
```

To build the Daffodil command line interface:

```bash 
$ sbt daffodil-cli/stage
```

The above will create Linux and Windows shell scripts in `daffodil-cli/target/universal/stage/bin/`. See the [Command Line Interface](https://daffodil.apache.org/cli/) documentation for details on its usage.

To run the Apache RAT checks:

```bash
$ sbt ratCheck
```

This will generate a RAT report in ``target/rat.txt`` and will error if any unapproved licenses are found.

### Debugging CLI Tests in Eclipse

The CLI tests in daffodil-cli execute a daffodil script to perform tests.
Before testing in Eclipse, you must first build that script by running ``sbt
daffodil-cli/stage``. Additionally, because the tests spawn a sub-shell for
executing the script, tests must be individually modified to make them
debuggable in Eclipse--the idiom is:

```scala
  // tests all synthesize a command line into val cmd
  val cmd = .....
  // Instead of running the rest of the test, do these two lines:
  val args = cmd.split(' ').tail // cmdline minus the daffodil program name.
  Main.run(args) // Run Main directly, so we can debug.
```

Note that this does not perform the 'expect' checks, so you have to type the
commands at the console pane, and inspect the output to see if it is what you
want.

## Getting Help

For questions, we can be reached at the dev@daffodil.apache.org or user@daffodil.apache.org mailing lists or in #Daffodil on [ASF HipChat](https://www.hipchat.com/gJt9EQs5l). Bugs can be reported via the [Daffodil JIRA](https://issues.apache.org/jira/projects/DAFFODIL).

## License

Daffodil is licensed under the [Apache License, v2.0](https://www.apache.org/licenses/LICENSE-2.0)
