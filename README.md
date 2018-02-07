# Apache Daffodil (incubating)

## Introduction

Apache Daffodil (incubating) is the open source implementation of the [Data Format Description Language (DFDL)](http://www.ogf.org/dfdl), a specification created by the [Open Grid Forum](http://www.ogf.org). DFDL is capable of describing many data formats, including textual and binary, commercial record-oriented, scientific and numeric, modern and legacy, and many industry standards. It leverages XML technology and concepts, using a subset of W3C XML schema type system and annotations to describe such data. Daffodil uses this description to parse data into an infoset represented as XML or JSON, easily capable of ingestion, validation, and transformation.

For more information about Daffodil, see https://daffodil.apache.org/.

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
$ sbt daffodil-cli/test
```

To build the Daffodil command line interface:

```bash 
$ sbt daffodil-cli/stage
```

The above will create Linux and Windows shell scripts in `daffodil-cli/target/universal/stage/bin/`. See the [Command Line Interface](https://daffodil.apache.org/cli/) documentation for details on its usage.

## Getting Help

For questions, we can be reached at the dev@daffodil.apache.org or user@daffodil.apache.org mailing lists or in #Daffodil on [ASF HipChat](https://www.hipchat.com/gJt9EQs5l). Bugs can be reported via the [Daffodil JIRA](https://issues.apache.org/jira/projects/DAFFODIL).

## License

Daffodil is licensed under the [University of Illinois/NCSA Open Source License.](https://opensource.org/licenses/NCSA)
