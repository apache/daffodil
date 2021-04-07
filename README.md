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
[<img src="https://img.shields.io/github/workflow/status/apache/daffodil/Daffodil%20CI/master.svg" align="right"/>][GitHub Actions]
<br clear="right" />
[<img src="https://img.shields.io/codecov/c/github/apache/daffodil/master.svg" align="right"/>][CodeCov]
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

For more information about Daffodil, see the [Website].

## Build Requirements

* JDK 8 or higher
* SBT 0.13.8 or higher
* C compiler C99 or higher
* Mini-XML Version 3.2 or higher

Since Daffodil has a DFDL to C backend, you will need a C compiler
([gcc] or [clang]), the [Mini-XML] library, and possibly the GNU
[argp] library if your system's C library doesn't include it.  You can
install gcc and libmxml as system packages on most Unix based
platforms with distribution-specific packager commands such as (Debian
and Ubuntu):

    # Just mentioning all other packages you might need too
    sudo apt install build-essential curl git libmxml-dev

You will need the Java Software Development Kit ([JDK]) and the Scala
Build Tool ([SBT]) to build Daffodil, run all tests, create packages,
and more.  [SDK] offers an easy and uniform way to install both java
and sbt on any Unix based platform:

    curl -s "https://get.sdkman.io" | bash
    sdk install java
    sdk install sbt

You can edit the Compile / cCompiler setting in build.sbt if you don't
want sbt to call your C compiler with "cc" as the driver command.

On Windows, the easiest way to install gcc and libargp is to install
[MSYS2]'s collection of free tools and libraries although MSYS2 has no
package for libmxml which you'll need to build from source.  First
install [MSYS2] following its website's installation instructions,
then run the following commands in a "MSYS2 MSYS" window:

    pacman -S gcc git libargp-devel make pkgconf
    git clone https://github.com/michaelrsweet/mxml.git
    cd mxml
    ./configure --prefix=/usr --disable-shared --disable-threads
    make
    make install

You also need to install [JDK} and [SBT] from their Windows
installation packages and define an environment variable using
Windows' control panel for editing environment variables.  Define an
environment variable with the name `MSYS2_PATH_TYPE` and the value
`inherit`.  Now when you open a new "MSYS2 MSYS" window from the Start
Menu, you will be able to type your sbt commands in the MSYS2 window
and both sbt and daffodil will be able to call the C compiler.

## Getting Started

Below are some of the more common commands used for Daffodil development.

### Compile

    sbt compile

### Tests

Run all unit tests:

    sbt test

Run all command line interface tests:

    sbt it:test

### Command Line Interface

Create Linux and Windows shell scripts in
`daffodil-cli/target/universal/stage/bin/`.  See the [Command Line
Interface] documentation for details on its usage:

    sbt daffodil-cli/stage

### License Check

Generate an [Apache RAT] license check report located in
``target/rat.txt`` and error if any unapproved licenses are found:

    sbt ratCheck

### Test Coverage Report

Generate an [sbt-scoverage] test coverage report located in
``target/scala-ver/scoverage-report/``:

    sbt clean coverage test it:test
    sbt coverageAggregate

## Getting Help

For questions, we can be reached at the dev@daffodil.apache.org or
users@daffodil.apache.org mailing lists.  Bugs can be reported via the
[Daffodil JIRA].

## License

Apache Daffodil is licensed under the [Apache License, v2.0].

[Apache License, v2.0]: https://www.apache.org/licenses/LICENSE-2.0
[Apache RAT]: https://creadur.apache.org/rat/
[CodeCov]: https://app.codecov.io/gh/apache/daffodil
[Command Line Interface]: https://daffodil.apache.org/cli/
[DFDL specification]: https://daffodil.apache.org/docs/dfdl/
[Daffodil JIRA]: https://issues.apache.org/jira/projects/DAFFODIL/
[Github Actions]: https://github.com/apache/daffodil/actions?query=branch%3Amaster+
[JDK]: https://adoptopenjdk.net/
[Mini-XML]: https://www.msweet.org/mxml/
[MSYS2]: https://www.msys2.org/
[Releases]: http://daffodil.apache.org/releases/
[SBT]: https://www.scala-sbt.org/
[SDK]: https://sdkman.io/
[Website]: https://daffodil.apache.org/
[argp]: https://packages.msys2.org/package/libargp-devel
[clang]: https://clang.llvm.org/get_started.html
[gcc]: https://linuxize.com/post/how-to-install-gcc-on-ubuntu-20-04/
[sbt-scoverage]: https://github.com/scoverage/sbt-scoverage/
