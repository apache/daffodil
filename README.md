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

[<img src="https://daffodil.apache.org/assets/themes/apache/img/apache-daffodil-logo.svg" height="85" align="left" alt="Apache Daffodil"/>][Website]
[<img src="https://img.shields.io/github/workflow/status/apache/daffodil/Daffodil%20CI/master.svg" align="right"/>][GitHub Actions]
<br clear="right" />
[<img src="https://img.shields.io/codecov/c/github/apache/daffodil/master.svg" align="right"/>][CodeCov]
<br clear="right" />
[<img src="https://img.shields.io/maven-central/v/org.apache.daffodil/daffodil-core_2.12.svg?color=brightgreen&label=version" align="right"/>][Releases]
<br clear="both" />

Apache Daffodil is an open-source implementation of the [DFDL specification]
that uses DFDL data descriptions to parse fixed format data into an infoset.
This infoset is commonly converted into XML or JSON to enable the use of
well-established XML or JSON technologies and libraries to consume, inspect,
and manipulate fixed format data in existing solutions. Daffodil is also
capable of serializing or "unparsing" data back to the original data format.
The DFDL infoset can also be converted directly to/from the data structures
carried by data processing frameworks so as to bypass any XML/JSON overheads.

For more information about Daffodil, see https://daffodil.apache.org/.

## Build Requirements

* JDK 8 or higher
* SBT 0.13.8 or higher

## Getting Started

[SBT] is the officially supported tool to build Daffodil, run all tests, create packages,
and more. Below are some of the more common commands used for Daffodil development.

### Compile

```text
$ sbt compile
```
### Tests

Run all unit tests:

```text
$ sbt test 
```

Run all command line interface tests:

```text
$ sbt IntegrationTest/test
```

### Command Line Interface

Create Linux and Windows shell scripts in `daffodil-cli/target/universal/stage/bin/`. See
the [Command Line Interface] documentation for details on its usage:

```btext
$ sbt daffodil-cli/stage
```

### License Check

Generate an [Apache RAT] license check report located in ``target/rat.txt`` and error if
any unapproved licenses are found:

```text
$ sbt ratCheck
```

### Test Coverage Report

Generate an [sbt-scoverage] test coverage report located in
``target/scala-ver/scoverage-report/``:

```text
$ sbt clean coverage test IntegrationTest/test
$ sbt coverageAggregate
```

## Getting Help

For questions, we can be reached at the dev@daffodil.apache.org or
users@daffodil.apache.org mailing lists. Bugs can be reported via the [Daffodil JIRA].

## License

Apache Daffodil is licensed under the [Apache License, v2.0].




[Apache License, v2.0]: https://www.apache.org/licenses/LICENSE-2.0
[Apache RAT]: https://creadur.apache.org/rat/
[CodeCov]: https://codecov.io/gh/apache/daffodil/
[Command Line Interface]: https://daffodil.apache.org/cli/
[Daffodil JIRA]: https://issues.apache.org/jira/projects/DAFFODIL
[DFDL specification]: http://www.ogf.org/dfdl
[Open Grid Forum]: http://www.ogf.org
[Releases]: http://daffodil.apache.org/releases/
[SBT]: http://www.scala-sbt.org
[Github Actions]: https://github.com/apache/daffodil/actions?query=branch%3Amaster+
[Website]: https://daffodil.apache.org
[sbt-scoverage]: https://github.com/scoverage/sbt-scoverage
