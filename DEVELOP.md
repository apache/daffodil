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

# Daffodil Developer Guide

This guide is for a software engineer who wants to develop Daffodil
code. This guide will help you become familiar with DFDL schemas,
DFDL processors, and Daffodil development including its code style,
code workflow, development environment, directory organization,
documentation, and tests.

## DFDL

The [Data Format Description Language][] (DFDL) is a language used to
describe almost all data formats both logically and physically. DFDL
is not a data format or a procedural language; rather, it is a data
modeling language based on a subset of XML Schema annotated with DFDL
properties describing the representation and layout of each element of
the schema inside a native text or binary data format. A DFDL schema
allows data to be converted between its native data format (physical
representation, also called a text or binary file) and a DFDL
information set (logical representation, also called an infoset) such
as an XML document, EXI document, JSON document, SAX callbacks, or
several document object model APIs in memory (JDOM, Scala Node, W3C
DOM). When you have a DFDL schema for a native data format, you can
pick whichever infoset type is easiest for you or your application to
use and tell a DFDL processor to read ("parse") a text or binary file
from its native data format to that infoset type. You or your
application can do whatever you need to do with the infoset and then
you can use the same DFDL schema and DFDL processor to write
("unparse") the infoset back to its native text or binary file format
again, completing a round trip from native data to infoset to native
data again.

Using DFDL avoids inventing a completely new data modeling language,
avoids writing any parsing and serialization code (with all the bugs
that normally arise from implementing such code procedurally), and
makes it much easier to convert any native data format to an infoset,
operate on the infoset, and convert an infoset back to its native data
format again.

To learn more about DFDL, you can watch two short [videos][DFDL
Getting Started Videos] put together by Steve Hanson, co-chair of the
DFDL working group, read a [slideshow][DFDL Overview Slides] written
by Mike Beckerle, co-chair of the DFDL working group, or go through
some [tutorials][DFDL Tutorials] written by Roger Costello, chair of
the DFTVL working group (Data Format Transformation and Validation
Language, a future not-yet-defined language to specify policies for
Cross-Domain Solution (CDS) devices).

## Apache Daffodil, IBM DFDL, and ESA DFDL4S

The standards organization in which DFDL started, the Open Grid Forum,
required 2 implementations in order to move forward with the
standardization process. This means that there are two leading DFDL
processors, a commercial implementation bundled into [IBM Integration
Bus][] called IBM DFDL and an open source implementation hosted by the
Apache Software Foundation (ASF) called [Apache Daffodil][Daffodil
website]. The European Space Agency also has created a proprietary
implementation called [ESA DFDL4S][], which can be used only with
their satellite communication formats and is provided only in the form
of binary libraries, not source code.

Among these three DFDL processors, Apache Daffodil is considered the
most modern and thorough implementation of the [Data Format
Description Language Specification][DFDL Specification]. Even so,
Apache Daffodil lists some [unsupported
features](https://daffodil.apache.org/unsupported/) of the DFDL
specification. IBM DFDL lists some more [unsupported
features](https://www.ibm.com/docs/en/integration-bus/10.0?topic=dfdl-unsupported-features)
and also lists some [implementation-specific
limits](https://www.ibm.com/docs/en/integration-bus/10.0?topic=dfdl-implementation-specific-limits).
These limitations will not prevent you from writing DFDL schemas for
almost all data formats, but they will reveal which parts of the DFDL
specification are rarely used.

## Daffodil Development

The Apache Software Foundation hosts the Apache Daffodil project on
the following ASF infrastructure:

- Daffodil's [issue tracker][Daffodil JIRA] is hosted on JIRA
- Daffodil's [users][Daffodil@users], [dev][Daffodil@dev], and
  [commits][Daffodil@commits] mailing list archives are hosted on
  Apache Pony Mail (subscription is required to send to these lists)
- Daffodil's [source code][Daffodil source] is hosted on GitHub
- Daffodil's [wiki][Daffodil Confluence] is hosted on Confluence
- Daffodil's [website][Daffodil website] is hosted on Apache with
  static content generated by Jekyll using jekyll-asciidoc and
  asciidoctor-diagram plugins

A good Daffodil developer gets an account on the Apache Software
Foundation's JIRA and Confluence servers, subscribes to all three
Daffodil mailing lists, and forks Daffodil's source code repositories
to the developer's GitHub account.

Only Daffodil developers given "committer" permission by the Daffodil
Project Management Committee have direct write access to Daffodil's
source code repositories. New Daffodil contributors have to fork the
[Daffodil source][] repository to their own GitHub account and then
clone their working copy from their fork as described in the [Code
Contributor Workflow][], which has all the steps you need to make a
successful code contribution to Daffodil. The same workflow applies
to Apache Daffodil's other two source code repositories, that is,
always fork these repositories and clone from your forks before you
make any changes to the [Daffodil VS Code Extension][Daffodil vscode]
or [Daffodil Website][Daffodil site] files.

The following advice is for Daffodil committers who do have write
access to Daffodil's source repositories. If you clone an Apache
repository directly to your computer, it is very important you
maintain that clone as a reading copy clearly separated from any clone
of your forked repository. The only commands you should ever run in
your reading copy are `git pull`, `git log ORIG_HEAD..HEAD`, and `git
diff ORIG_HEAD..HEAD` in order to see what changes have been made by
other developers since your last pull from the Apache repository.
Your reading copy should remain an exact copy of the Apache source
code repository at all times which you can use for reading or
occasionally running `diff` commands between your reading copy and
working copy. To be safe, rename your reading copy from `daffodil` to
`daffodil-asf` and never edit files or run `sbt` in it so there will
be nothing to push to the Apache repository even if you accidentally
run `git push` from your reading copy.

### Code Style

Daffodil code is formatted using [Scalafmt](https://scalameta.org/scalafmt/).
Run `sbt scalafmtCheckAll` to see if changes to the source code require
formatting and then run `sbt scalafmtAll` to apply the changes. For
SBT files use `sbt scalafmtSbtCheck` and `sbt scalafmtSbt`.

Daffodil mandates that at least 80% of new code and modified code
should be covered by unit tests or TDML tests. Daffodil naming
conventions and more are covered in the Confluence pages [Code Style
Guidelines][] and [Coding for Performance][].

### Code Workflow

The [Code Contributor Workflow][] has all the steps you need to make a
successful code contribution to Daffodil. Here are some more things
you should know too.

The Daffodil developers want each pull request to add only one commit
to the Apache repository to keep its git history as readable and
bisectable as possible. Your pull request should fix a JIRA issue in
its entirety (including test cases to cover any modified or new lines)
but it should not fix 2 or more unrelated JIRA issues in the same
commit either. Your pull request should start with only one commit in
it before you ask the developers to review it and your pull request
should end with only one commit in it after you receive enough
reviewer approvals (two +1's for Daffodil and Daffodil VS Code
Extension, one +1 for Daffodil Website). If you have received your
reviewers' approval but you have made multiple commits to your pull
request due to reviewer suggestions (which is the usual case), you
must rebase and squash your pull request back to one commit before a
Daffodil committer can merge your code into any of the upstream Apache
repositories.

Before you change Daffodil code, you will need to know how to use JIRA
as well as GitHub. You will assign a JIRA issue to yourself before
you start working on it to avoid redundant effort in case another
developer decides to work on the same issue. Then you will mark the
issue as resolved after your pull request is merged. Here are the
steps you need to perform in JIRA to resolve your issue after your
pull request is merged (please don't forget these steps):

1. Click "Resolve Issue"
2. Click the "Fix Version/s" dropdown and mark the issue as fixed in
   the currently unreleased version
3. Paste "Fixed in commit xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
   into the comment field (replacing the xxx's with the correct hash)
4. Press the "Resolve" button

That's it. You can expect someone else to close the JIRA issue so you
don't need to, and should not, close the issue yourself. Just make
sure your pull request has tests if appropriate so that someone can
look at your pull request and verify that it resolves the JIRA issue.

### Development Environment

You will find build instructions in [BUILD.md](BUILD.md) and
[README.md](README.md). You will find more instructions how to set up
your development environment in the Confluence pages [Java Setup][]
and [IntelliJ IDEA Setup][].

### Directory Organization

Daffodil is composed of one top-level project and numerous
subdirectory-level modules (some of these modules contain only
generators, macros, tests and tutorials). Daffodil's source code
repository is organized as follows:

```text
daffodil/
├── .asf.yaml                   - Enables/disables parts of GitHub's UI
├── .gitattributes              - Excludes KEY from source releases
├── .gitignore                  - Ignores auto-generated files from source control
├── .sbtopts                    - Tells sbt to use 4GB of memory
├── BUILD.md                    - Lists Daffodil's build requirements
├── KEYS                        - Lists public keys of Daffodil developers
├── LICENSE                     - Licenses Daffodil under the Apache License
├── NOTICE                      - Lists notices affecting LICENSE
├── README.md                   - Explains how to get started
├── build.sbt                   - Tells sbt how to build Daffodil
├── containers/
│   └── release-candidate       - Defines Docker image used to build release
├── daffodil-cli/
│   ├── README.md               - Explains how to start Daffodil
│   ├── bin.LICENSE             - Contains licenses of Daffodil and subcomponents
│   ├── bin.NOTICE              - Contains notices of Daffodil and subcomponents
│   ├── build.sbt               - Tells sbt how to build CLI and installers
│   └── src/                    - Contains CLI source code, tests, scripts, etc.
├── daffodil-codegen-c/         - Contains Daffodil's C code generator
├── daffodil-macro-lib/         - Defines Daffodil's Scala 2.x macros
├── daffodil-propgen/           - Contains generators to generate more source code
├── daffodil-runtime1/          - Contains Daffodil's input/output/charsets, Scala parser, schema compiler, utilities, layers, unparsers and the API
├── daffodil-schematron/
│   ├── README.md               - Explains how to validate with Schematron rules
│   └── src/                    - Contains Daffodil's Schematron validator
├── daffodil-tdml-lib/          - Contains Daffodil's TDML runner
├── daffodil-tdml-processor/    - Contains Daffodil's Scala & C TDML implementations
├── daffodil-test/              - Contains Daffodil's TDML tests
├── daffodil-test-ibm1/         - Contains more TDML tests from IBM & Tresys
├── daffodil-udf/
│   ├── README.md               - Explains how to implement user defined functions
│   └── src/                    - Contains Daffodil's UDF interfaces
├── project/
│   ├── Dependencies.scala      - Defines third party dependencies
│   ├── Rat.scala               - Lists files to be ignored by license audit tool
│   ├── build.properties        - Defines which sbt version to use
│   └── plugins.sbt             - Adds third party plugins to sbt
├── test-stdLayout/             - Contains TDML tests laid out in different ways
└── tutorials/                  - Contains Daffodil's TDML test tutorials
```

When compiled into a command line interface, Daffodil is composed of a script
and a lib directory containing all Daffodil jars and third-party dependency
jars needed by the Daffodil CLI. Daffodil's command line interface is released
in binary form as follows:

```text
apache-daffodil-3.4.0-bin/
├── LICENSE          - Contains licenses of Daffodil and subcomponents
├── NOTICE           - Contains notices of Daffodil and subcomponents
├── README.md        - Explains how to start Daffodil
├── bin/
│   ├── daffodil     - Starts Daffodil on Linux
│   └── daffodil.bat - Starts Daffodil on Windows
└── lib/             - Contains Daffodil jars and third party jars
```

When an application uses Daffodil's Java or Scala API, it will need to
put fewer Daffodil jars and third-party dependency jars on its
classpath (omitting the jars needed only by Daffodil's CLI). The best
way for a Java or Scala application to use Daffodil is to get
Daffodil's Java API from Maven or get Daffodil's Scala API from sbt:

```xml
<dependency>
  <groupId>org.apache.daffodil</groupId>
  <artifactId>daffodil-runtime1_3.3</artifactId>
  <version>3.4.0</version>
</dependency>
```

```sbt
libraryDependencies += "org.apache.daffodil" %% "daffodil-runtime1" % "4.0.0"
```

When you use Daffodil's C code generator to generate C code from a
DFDL schema, the newly created C code directory is organized as
follows:

```text
c/
├── Makefile              - Contains build and test targets
├── libcli/
│   ├── cli_errors.c      - Implements CLI error messages
│   ├── daffodil_getopt.c - Implements CLI options
│   ├── daffodil_main.c   - Starts the CLI
│   ├── stack.c           - Pushes XML nodes on a stack
│   ├── xml_reader.c      - Reads an infoset from a XML file
│   └── xml_writer.c      - Writes an infoset to a XML file
├── libruntime/
│   ├── errors.c          - Implements error messages
│   ├── generated_code.c  - Implements functions to create/use infoset
│   ├── infoset.c         - Implements functions to walk infoset
│   ├── parsers.c         - Implements functions to read binary data
│   └── unparsers.c       - Implements functions to write binary data
└── tests/
    ├── bits.c            - Tests integers with different lengths
    └── extras.c          - Avoids an undefined reference when linking
```

You can use the Makefile to build a Daffodil executable (c/daffodil)
and run c/daffodil with similar parse and unparse options as
Daffodil's Scala command line interface. If you want to embed the
generated C code into your own C application, you can call only
libruntime to read and write binary data into C structs in memory or
call both libruntime and libcli (replacing daffodil_main.c with your
application's main.c) to read and write an XML infoset as well as
binary data into C structs.

### Documentation

Unfortunately, it is not possible to search or edit all of Daffodil's
documentation in one single place. Daffodil documentation is split
between the [Daffodil Confluence][] wiki, the [Daffodil source][]
repository, and the [Daffodil site][] repository. Confluence has a
page tree on the left side so you can find all the Confluence pages by
expanding the page tree. The first link below is the only link you
need to find or search all the pages in the wiki. However, there are
a lot of pages in the wiki so here are some of the most important
pages worth reading as well (sorted in alphabetical order).

- [Apache Daffodil Wiki][Daffodil Confluence]
- [Code Contributor Workflow][]
- [Code Style Guidelines][]
- [Coding for Performance][]
- [DFDL Schema Object Model][]
- [Daffodil Maturity Model Assessment][]
- [Daffodil Code Generators][]
- [IntelliJ IDEA Setup][]
- [Object-Oriented Lazy Attribute Grammars][]
- [Roadmap for Upcoming Releases][]
- [Scala References and Patterns][]
- [sbt Tips and Tricks][]

Some of the Daffodil website pages don't seem to have any links
pointing to them from other website pages. So that you can read all
of the website pages in your browser more easily, here are direct
links to every website page (sorted in alphabetical order).

- [A TDML Tutorial][]
- [About AsciiDoc for Documentation][]
- [About TDML Tutorials][]
- [Bug Report TDML Template][]
- [Command Line Interface][]
- [Community][]
- [Configuration][]
- [DFDL Extensions][]
- [Daffodil Java API][]
- [Daffodil Scala API][]
- [Daffodil and the DFDL Infoset][]
- [DaffodilC ToDos][]
- [Data Format Description Language v1.0 Specification][]
- [Eclipse Configuration][]
- [Examples][]
- [Frequently Asked Questions][]
- [Getting Started][]
- [Hidden Groups][]
- [Home][]
- [Infoset Inputters and Runtime 1 Streaming Unparser & NextElementResolver][]
- [Interactive Debugger][]
- [Namespace Binding Minimization][]
- [People][]
- [Releases][]
- [Standard DFDL Schema Project Layout][]
- [Term Sharing in the Schema Compiler][]
- [Test Data Markup Language][]
- [Understanding dfdl:bitOrder][]
- [Unsupported Features and Errata][]
- [VS Code Extension][]
- [design notes folder][]

### Tests

The majority of Daffodil tests are written inside TDML files with
corresponding Scala unit tests which allow your IDE or sbt to run
these tests within the TDML files. Daffodil also has some pure Scala
unit tests for testing smaller parts of Daffodil at a lower level.
Both kinds of tests are important, although new Daffodil tests tend to
be written in TDML and focus on testing issues with Daffodil's
processing of DFDL schemas and parsing data or unparsing data. You
can run both kinds of tests with sbt using this command:

```text
sbt test
```

As mentioned in [sbt Tips and Tricks][], you can replace `test` with
`testOnly` and arguments to run a smaller number of tests. At the
most extreme, you can run only one test method in one Scala test file,
or you can use either wildcards or multiple arguments to filter which
tests are run. It is easier to type such `testOnly` commands at the
sbt prompt since you will need to enclose the entire `testOnly`
command including its arguments in quotes if you type it at the
terminal's command line prompt. Nevertheless, here are examples of
how to enclose a testOnly command and its arguments in quotes on the
command line:

```text
sbt "daffodil-test/testOnly org.apache.daffodil.section05.facets.TestNulChars"
sbt "daffodil-test-integration/testOnly org.apache.daffodil.cliTest.TestCLIUdfs -- --tests=test_noUdfsLoaded_regular_schema"
```

TDML tests are helpful for Daffodil developers because they allow a
developer to write a TDML file with a schema, a test case, and test
data which isolates a complex data format issue so the developer can
work on that issue more easily. The developer is free to decide
whether to define the schema and test data inline in the TDML file or
leave the schema and test data in separate files which the TDML file
reads. For easier development, you can run TDML tests in the same
directory without needing your IDE or sbt using this command:

```text
daffodil test nums.tdml
```

The developer also can choose which TDML implementation Daffodil
should use to run a TDML test, either Daffodil's Scala backend or
Daffodil's C code generator, simply by passing an option to Daffodil's
test command or passing a TDMLImplementation instance to Daffodil's
TDML Runner class in a Scala test class. The developer can use a TDML
test to work on a part of DFDL which needs to be supported by the C
code generator and keep running (or debugging) the TDML test until the
generated C code works successfully:

```text
daffodil test -I daffodilC nums.tdml
```

Daffodil also has command line interface tests in the daffodil-cli and
daffodil-test-integration modules. Slower CLI tests that require
forking should go in the latter module. If a developer changes any
part of Daffodil's command line interface, the developer should test
that change in one of these modules. You can run all CLI tests
using these sbt commands:

```text
sbt daffodil-cli/test
sbt daffodil-test-integration/test
```

Daffodil also has a [GitHub Actions][] continuous integration workflow
which builds the codebase and runs all the tests every time a pull
request is changed or merged into the repository. The pull request
will display whether the CI checks succeeded or failed, so Daffodil
developers will have to become familiar with troubleshooting these CI
checks whenever their pull request fails a CI check. For example, one
of the CI checks generates a test coverage analysis report and fails
if the pull request has less than 80% coverage. Another CI check
generates a SonarCloud quality report too. Here are current and past
workflow runs on GitHub, the current quality report on SonarCloud, and
the current test coverage report on Codecov:

- [GitHub Actions Workflow Runs][]
- [SonarCloud Quality Report][]
- [Test Coverage Analysis Report][]

<!-- Define reference links here to make text sections shorter and -->
<!-- easier to edit -->

<!-- General links -->

[DFDL Getting Started Videos]: https://community.ibm.com/community/user/integration/viewdocument/get-started-with-the-data-format-de

[DFDL Overview Slides]: https://www.slideshare.net/mbeckerle/dfdl-and-apache-daffodil-overview-from-owl-cyber-defense

[DFDL Specification]: https://daffodil.apache.org/docs/dfdl/

[DFDL Tutorials]: http://www.xfront.com/DFDL/

[Daffodil Confluence]: https://cwiki.apache.org/confluence/display/DAFFODIL/

[Daffodil JIRA]: https://issues.apache.org/jira/projects/DAFFODIL/

[Daffodil website]: https://daffodil.apache.org/

[Data Format Description Language]: https://en.wikipedia.org/wiki/Data_Format_Description_Language

[ESA DFDL4S]: https://eop-cfi.esa.int/index.php/applications/dfdl4s

[IBM Integration Bus]: https://www.ibm.com/docs/en/integration-bus/10.0?topic=model-data-format-description-language-dfdl

<!-- Daffodil Confluence links -->

[Code Contributor Workflow]: https://cwiki.apache.org/confluence/display/DAFFODIL/Code+Contributor+Workflow

[Code Style Guidelines]: https://cwiki.apache.org/confluence/display/DAFFODIL/Code+Style+Guidelines

[Coding for Performance]: https://cwiki.apache.org/confluence/display/DAFFODIL/Coding+for+Performance

[DFDL Schema Object Model]: https://cwiki.apache.org/confluence/display/DAFFODIL/DFDL+Schema+Object+Model+%28DSOM%29+with+UML

[Daffodil Maturity Model Assessment]: https://cwiki.apache.org/confluence/display/DAFFODIL/Apache+Daffodil+Maturity+Model+Assessment

[Daffodil Code Generators]: https://cwiki.apache.org/confluence/display/DAFFODIL/Daffodil+Code+Generators

[IntelliJ IDEA Setup]: https://cwiki.apache.org/confluence/display/DAFFODIL/IntelliJ+IDEA+Setup

[Java Setup]: https://cwiki.apache.org/confluence/display/DAFFODIL/Java+Setup+and+Notes

[Object-Oriented Lazy Attribute Grammars]: https://cwiki.apache.org/confluence/display/DAFFODIL/OOLAG+-+Object-Oriented+Lazy+Attribute+Grammars

[Roadmap for Upcoming Releases]: https://cwiki.apache.org/confluence/display/DAFFODIL/Roadmap+for+Upcoming+Releases

[Scala References and Patterns]: https://cwiki.apache.org/confluence/display/DAFFODIL/Scala+References+and+Patterns

[sbt Tips and Tricks]: https://cwiki.apache.org/confluence/display/DAFFODIL/sbt+Tips+and+Tricks

<!-- Daffodil GitHub links -->

[Daffodil site]: https://github.com/apache/daffodil-site

[Daffodil source]: https://github.com/apache/daffodil

[Daffodil vscode]: https://github.com/apache/daffodil-vscode

[GitHub Actions]: https://docs.github.com/en/actions

<!-- Daffodil JIRA links -->

[DAFFODIL-2133]: https://issues.apache.org/jira/browse/DAFFODIL-2133

<!-- Daffodil archives/reports links -->

[Daffodil@commits]: https://lists.apache.org/list.html?commits@daffodil.apache.org

[Daffodil@dev]: https://lists.apache.org/list.html?dev@daffodil.apache.org

[Daffodil@users]: https://lists.apache.org/list.html?users@daffodil.apache.org

[GitHub Actions Workflow Runs]: https://github.com/apache/daffodil/actions

[SonarCloud Quality Report]: https://sonarcloud.io/project/overview?id=apache-daffodil

[Test Coverage Analysis Report]: https://app.codecov.io/gh/apache/daffodil

<!-- Daffodil website links -->

[A TDML Tutorial]: https://daffodil.apache.org/tutorials/tdmlTutorial.tdml.xml

[About AsciiDoc for Documentation]: https://daffodil.apache.org/dev/aboutAsciiDoc/

[About TDML Tutorials]: https://daffodil.apache.org/assets/tutorials/aboutTDMLTutorials/

[Bug Report TDML Template]: https://daffodil.apache.org/tutorials/bugReportTemplate.tdml.xml

[Command Line Interface]: https://daffodil.apache.org/cli/

[Community]: https://daffodil.apache.org/community/

[Configuration]: https://daffodil.apache.org/configuration/

[DFDL Extensions]: https://daffodil.apache.org/dfdl-extensions/

[Daffodil Java API]: https://daffodil.apache.org/docs/latest/javadoc/

[Daffodil Scala API]: https://daffodil.apache.org/docs/latest/scaladoc/

[Daffodil and the DFDL Infoset]: https://daffodil.apache.org/infoset/

[DaffodilC ToDos]: https://daffodil.apache.org/dev/design-notes/daffodilc-todos/

[Data Format Description Language v1.0 Specification]: https://daffodil.apache.org/docs/dfdl/

[Eclipse Configuration]: https://daffodil.apache.org/eclipse-configuration/

[Examples]: https://daffodil.apache.org/examples/

[Frequently Asked Questions]: https://daffodil.apache.org/faq/

[Getting Started]: https://daffodil.apache.org/getting-started/

[Hidden Groups]: https://daffodil.apache.org/dev/design-notes/hidden-groups/

[Home]: https://daffodil.apache.org/

[Infoset Inputters and Runtime 1 Streaming Unparser & NextElementResolver]: https://daffodil.apache.org/dev/design-notes/infoset-inputter-streaming-unparser/

[Interactive Debugger]: https://daffodil.apache.org/debugger/

[Namespace Binding Minimization]: https://daffodil.apache.org/dev/design-notes/namespace-binding-minimization/

[People]: https://daffodil.apache.org/people/

[Releases]: https://daffodil.apache.org/releases/

[Standard DFDL Schema Project Layout]: https://daffodil.apache.org/dfdl-layout/

[Term Sharing in the Schema Compiler]: https://daffodil.apache.org/dev/design-notes/term-sharing-in-schema-compiler/

[Test Data Markup Language]: https://daffodil.apache.org/tdml/

[Understanding dfdl:bitOrder]: https://daffodil.apache.org/tutorials/bitorder.tutorial.tdml.xml

[Unsupported Features and Errata]: https://daffodil.apache.org/unsupported/

[VS Code Extension]: https://daffodil.apache.org/vscode/

[design notes folder]: https://daffodil.apache.org/dev/
