# Daffodil

## Introduction

Daffodil is the open source implementation of the [Data Format Description Language (DFDL)](http://www.ogf.org/dfdl), a specification created by the [Open Grid Forum](http://www.ogf.org). DFDL is capable of describing many data formats, including textual and binary, commercial record-oriented, scientific and numeric, modern and legacy, and many industry standards. It leverages XML technology and concepts, using a subset of W3C XML schema type system and annotations to describe such data. Daffodil uses this description to parse data into an infoset represented as XML or JSON, easily capable of ingestion, validation, and transformation.

For more information about Daffodil, see the [Daffodil Wiki](https://opensource.ncsa.illinois.edu/confluence/display/DFDL/Daffodil%3A+Open+Source+DFDL).

## Build Requirements

* JDK 1.8
* Scala 2.11.8
* SBT 0.13.8

## Getting Started

To build Daffodil:

```bash
$ sbt clean package
```
To run all Daffodil tests:

```bash
# run all unit tests
$ sbt test 
# run all command line tests
$ sbt cli
```

To build the Daffodil CLI:

```bash 
$ sbt stage
```

The above will create Linux and Windows shell scripts in `daffodil-cli/target/universal/stage/bin/`. See the [Daffodil CLI](https://opensource.ncsa.illinois.edu/confluence/display/DFDL/Command+Line+Interface) for details on its usage.

## Getting Help

For questions, we can be reached on the [Daffodil users mailing list](http://oss.tresys.com/mailman/listinfo/daffodil-users) or in the #Daffodil room on [NCSA HipChat](http://hipchat.ncsa.illinois.edu/gvZdmJHmq). Bugs can be reported via the [Daffodil JIRA](https://opensource.ncsa.illinois.edu/jira/projects/DFDL/), or via email at [daffodil-fouo-support@tresys.com](mailto:daffodil-fouo-support@tresys.com) for company confidential, FOUO, or security relevant issues.

## License

Daffodil is licensed under the [University of Illinois/NCSA Open Source License.](https://opensource.org/licenses/NCSA)
