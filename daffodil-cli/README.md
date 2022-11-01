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

# Apache Daffodil

## Introduction

Apache Daffodil is an open-source implementation of the DFDL specification
that uses DFDL data descriptions to parse fixed format data into an infoset.
This infoset is commonly converted into XML or JSON to enable the use of
well-established XML or JSON technologies and libraries to consume, inspect,
and manipulate fixed format data in existing solutions. Daffodil is also
capable of serializing or "unparsing" data back to the original data format.
The DFDL infoset can also be converted directly to/from the data structures
carried by data processing frameworks so as to bypass any XML/JSON overheads.

For more information about Daffodil, see <https://daffodil.apache.org/>.

## Requirements

* JRE 8 or higher

## Getting Started

To execute Daffodil on Linux:

```bash
./bin/daffodil [options]
```

To execute Daffodil on Windows:

```bash
.\bin\daffodil.bat [options]
```

Use the `--help` option or see the [Command Line Interface](https://daffodil.apache.org/cli/)
documentation for details on its usage.

### Debugging

Daffodil comes with a built-in interactive debugger, allowing the user to pause
parse/unparse and inspect various aspects of the state. To enable the
interactive debugger, supply the global -d option when parsing or unparsing,
for example:

```bash
./bin/daffodil -d parse --schema <path/to/schema.dfdl.xsd> input-file
```

When running the debugger, the user is provided with a command prompt, at which
point the user can execute debugger commands to control the debugger and
inspect state. Type `help` at the command prompt to get information on the
debugger commands, or `help <command>` to get information about a specific
command.

See the [Interactive Debugger](https://daffodil.apache.org/debugger/)
page for its detailed usage.

## Getting Help

For questions, we can be reached at the dev@daffodil.apache.org or
users@daffodil.apache.org mailing lists. Bugs can be reported via the
[Daffodil JIRA](https://issues.apache.org/jira/projects/DAFFODIL).

## License

Daffodil is licensed under the [Apache License, v2.0](https://www.apache.org/licenses/LICENSE-2.0)

This product bundles the [Java Architecture for XML
Binding](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxb/index.html)
and the [JavaBeans Activation
Framework](https://www.oracle.com/java/technologies/downloads.html),
which are licensed under the Common Development and Distribution
License Version 1.1.

This product bundles the [Saxon XSLT and XQuery Processor from
Saxonica Limited](https://www.saxonica.com/), which is licensed under
the Mozilla Public License Version 2.0.
