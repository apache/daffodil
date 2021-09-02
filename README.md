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

<!-- markdownlint-disable first-line-heading -->
<!-- markdownlint-disable line-length -->
<!-- markdownlint-disable no-inline-html -->
[<img src="https://daffodil.apache.org/assets/themes/apache/img/apache-daffodil-logo.svg" height="85" align="left" alt="Apache Daffodil"/>][Website]
[<img src="https://img.shields.io/github/workflow/status/apache/daffodil/CI/main.svg" align="right"/>][GitHub Actions]
<br clear="right" />
[<img src="https://img.shields.io/codecov/c/github/apache/daffodil/main.svg" align="right"/>][CodeCov]
<br clear="right" />
[<img src="https://img.shields.io/maven-central/v/org.apache.daffodil/daffodil-core_2.12.svg?color=brightgreen&label=version" align="right"/>][Releases]
<br clear="both" />

Apache Daffodil is an open-source implementation of the [DFDL
specification] that uses DFDL data descriptions to parse fixed format
data into an infoset.  This infoset is commonly converted into XML or
JSON to enable the use of well-established XML or JSON technologies
and libraries to consume, inspect, and manipulate fixed format data in
existing solutions.  Daffodil is also capable of serializing or
"unparsing" data back to the original data format.  The DFDL infoset
can also be converted directly to/from the data structures carried by
data processing frameworks so as to bypass any XML/JSON overheads.

For more information about Daffodil, see <https://daffodil.apache.org/>.

## Build Requirements

* JDK 8 or higher
* SBT 0.13.8 or higher
* C compiler C99 or higher
* Mini-XML Version 3.0 or higher

See [BUILD.md](BUILD.md) for more details.

## Getting Started

[SBT] is the officially supported tool to build Daffodil.  Below are
some of the more commonly used commands for Daffodil development.

### Compile

Compile source code:

    sbt compile

### Tests

Run unit tests:

    sbt test

Run command line interface tests:

    sbt IntegrationTest/test

### Command Line Interface

Build the command line interface (Linux and Windows shell scripts in
`daffodil-cli/target/universal/stage/bin/`; see the [Command Line
Interface] documentation for details on their usage):

    sbt daffodil-cli/stage

### License Check

Run [Apache RAT] (license audit report in `target/rat.txt` and error
if any unapproved licenses are found):

    sbt ratCheck

### Test Coverage Report

Run [sbt-scoverage] (report in `target/scala-ver/scoverage-report/`):

    sbt clean coverage test IntegrationTest/test
    sbt coverageAggregate

## Getting Help

You can ask questions on the dev@daffodil.apache.org or
users@daffodil.apache.org mailing lists.  You can report bugs via the
[Daffodil JIRA].

## License

Apache Daffodil is licensed under the [Apache License, v2.0].

[Apache License, v2.0]: https://www.apache.org/licenses/LICENSE-2.0
[Apache RAT]: https://creadur.apache.org/rat/
[CodeCov]: https://app.codecov.io/gh/apache/daffodil
[Command Line Interface]: https://daffodil.apache.org/cli/
[DFDL specification]: https://daffodil.apache.org/docs/dfdl/
[Daffodil JIRA]: https://issues.apache.org/jira/projects/DAFFODIL/
[Github Actions]: https://github.com/apache/daffodil/actions?query=branch%3Amain+
[Releases]: http://daffodil.apache.org/releases/
[SBT]: https://www.scala-sbt.org/
[Website]: https://daffodil.apache.org/
[sbt-scoverage]: https://github.com/scoverage/sbt-scoverage/
