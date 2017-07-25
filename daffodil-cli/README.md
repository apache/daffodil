
# Daffodil

## Introduction

Daffodil is the open source implementation of the [Data Format Description Language (DFDL)](http://www.ogf.org/dfdl), a specification created by the [Open Grid Forum](http://www.ogf.org). DFDL is capable of describing many data formats, including textual and binary, commercial record-oriented, scientific and numeric, modern and legacy, and many industry standards. It leverages XML technology and concepts, using a subset of W3C XML schema type system and annotations to describe such data. Daffodil uses this description to parse data into an infoset represented as XML or JSON, easily capable of ingestion, validation, and transformation.

For more information about Daffodil, see the [Daffodil Wiki](https://opensource.ncsa.illinois.edu/confluence/display/DFDL/Daffodil%3A+Open+Source+DFDL).

## Requirements

* JDK 1.8

## Getting Started

To execute Daffodil on Linux:

```bash
$ ./bin/daffodil [options]
```

To execute Daffodil on Windows:

```bash
$ .\bin\daffodil.bat [options]
```

Use the `--help` option or see the [Daffodil CLI](https://opensource.ncsa.illinois.edu/confluence/display/DFDL/Command+Line+Interface) documentation for details on its usage.

### Debugging

Daffodil comes with a built-in interactive debugger, allowing the user to pause parse/unparse and inspect various aspects of the state. To enable the interactive debugger, supply the global -d option when parsing or unparsing, for example:

```bash
$ ./bin/daffodil -d parse --schema <path/to/schema.dfdl.xsd> input-file
```

When running the debugger, the user is provided with a command prompt, at which point the user can execute debugger commands to control the debugger and inspect state. Type `help` at the command prompt to get information on the debugger commands, or `help <command>` to get information about a specific command.

See the [Interactive Debugger](https://opensource.ncsa.illinois.edu/confluence/display/DFDL/Interactive+Debugger) page for its detailed usage.

## Getting Help

For questions, we can be reached on the [Daffodil users mailing list](http://oss.tresys.com/mailman/listinfo/daffodil-users) or in the #Daffodil room on [NCSA HipChat](http://hipchat.ncsa.illinois.edu/gvZdmJHmq). Bugs can be reported via the [Daffodil JIRA](https://opensource.ncsa.illinois.edu/jira/projects/DFDL/), or via email at [daffodil-fouo-support@tresys.com](mailto:daffodil-fouo-support@tresys.com) for company confidential, FOUO, or security relevant issues.

## License

Daffodil is licensed under the [University of Illinois/NCSA Open Source License.](https://opensource.org/licenses/NCSA)
