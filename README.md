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

* JDK 8 or higher
* SBT 0.13.8 or higher

## Getting Started

[SBT](http://www.scala-sbt.org) is the officially supported tool to build
Daffodil, run all tests, create packages, and more. Below are some of the more
common commands used for Daffodil development.

> Note that compiling and running all Daffodil tests works best with at least
> 6GB of memory, more than is usually provided by default. We recommended that
> you supply ``-mem 6144`` either as an sbt option (e.g. ``sbt -mem 6144
> test``) or edit ``/etc/sbt/sbtopts`` to increase the available memory when
> running the below commands.

### Compile

```bash
$ sbt compile
```
### Tests

The following command runs all unit tests.

```bash
$ sbt test 
```

The following command runs all command line interface tests.
```bash
$ sbt it:test
```

### Command Line Interface

The following command creates Linux and Windows shell scripts in `daffodil-cli/target/universal/stage/bin/`. See the [Command Line Interface](https://daffodil.apache.org/cli/) documentation for details on its usage.

```bash 
$ sbt daffodil-cli/stage
```

### Apache RAT

This following command generates an [Apache RAT](https://creadur.apache.org/rat/) report in ``target/rat.txt`` and errors if any unapproved licenses are found.

```bash
$ sbt ratCheck
```

## Getting Help

For questions, we can be reached at the dev@daffodil.apache.org or user@daffodil.apache.org mailing lists or in #Daffodil on [ASF HipChat](https://www.hipchat.com/gJt9EQs5l). Bugs can be reported via the [Daffodil JIRA](https://issues.apache.org/jira/projects/DAFFODIL).

## License

Daffodil is licensed under the [Apache License, v2.0](https://www.apache.org/licenses/LICENSE-2.0)
