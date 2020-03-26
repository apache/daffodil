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
[<img src="https://img.shields.io/travis/apache/incubator-daffodil/master.svg" align="right"/>][TravisCI]
<br clear="right" />
[<img src="https://img.shields.io/codecov/c/github/apache/incubator-daffodil/master.svg" align="right"/>][CodeCov]
<br clear="right" />
[<img src="https://img.shields.io/maven-central/v/org.apache.daffodil/daffodil-core_2.12.svg?color=brightgreen&label=version" align="right"/>][Releases]
<br clear="both" />

Apache Daffodil (incubating) is the open source implementation of the [Data Format
Description Language (DFDL)], a specification created by the [Open Grid Forum]. DFDL is
capable of describing many data formats, including textual and binary, commercial
record-oriented, scientific and numeric, modern and legacy, and many industry standards.
It leverages XML technology and concepts, using a subset of W3C XML schema type system and
annotations to describe such data. Daffodil uses this description to parse data into an
infoset represented as XML or JSON, easily capable of ingestion, validation, and
transformation.

For more information about Daffodil, see https://daffodil.apache.org/.

## Build Requirements

* JDK 8 or higher
* SBT 0.13.8 or higher

## Getting Started

[SBT] is the officially supported tool to build Daffodil, run all tests, create packages,
and more. Below are some of the more common commands used for Daffodil development.

> :exclamation: **SBT Memory Requirements**
>
> *Compiling and running all Daffodil tests works best with at least 6GB of memory, more
> than is usually provided by default. We recommended that you supply ``-mem 6144`` either
> as an sbt option (e.g. ``sbt -mem 6144 test``) or edit ``/etc/sbt/sbtopts`` to increase
> the available memory when running the below commands.*

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
$ sbt it:test
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
$ sbt clean coverage test it:test
$ sbt coverageAggregate
```

## Getting Help

For questions, we can be reached at the dev@daffodil.apache.org or
users@daffodil.apache.org mailing lists. Bugs can be reported via the [Daffodil JIRA].

## License

Apache Daffodil is licensed under the [Apache License, v2.0].

## Disclaimer

Apache Daffodil is an effort undergoing incubation at The Apache Software Foundation
(ASF), sponsored by the Incubator PMC. Incubation is required of all newly accepted
projects until a further review indicates that the infrastructure, communications, and
decision making process have stabilized in a manner consistent with other successful ASF
projects. While incubation status is not necessarily a reflection of the completeness or
stability of the code, it does indicate that the project has yet to be fully endorsed by
the ASF.




[Apache License, v2.0]: https://www.apache.org/licenses/LICENSE-2.0
[Apache RAT]: https://creadur.apache.org/rat/
[CodeCov]: https://codecov.io/gh/apache/incubator-daffodil/
[Command Line Interface]: https://daffodil.apache.org/cli/
[Daffodil JIRA]: https://issues.apache.org/jira/projects/DAFFODIL
[Data Format Description Language (DFDL)]: http://www.ogf.org/dfdl
[Open Grid Forum]: http://www.ogf.org
[Releases]: http://daffodil.apache.org/releases/
[SBT]: http://www.scala-sbt.org
[TravisCI]: https://travis-ci.org/apache/incubator-daffodil
[Website]: https://daffodil.apache.org
[sbt-scoverage]: https://github.com/scoverage/sbt-scoverage
