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

Schematron Validation
===

Daffodil Validator API implementation for ISO Schematron in XSLT2

### Configuration

Parameters
- `schematron`: default parameter provides path to the schematron file or resource

### Parse Results

- `SchematronValidationError` on failed rule check

### SPI registration

Registered as `schematron`  

`Validators.get("schematron")`

### Command Line Usage

The `schematron` argument to the `--validate` flag will provide the schematron parameter and initiate the parsing.

`daffodil parse --validate schematron=my.sch ...`

### References

- [Camel Schematron Component](https://github.com/apache/camel/tree/4ea9e6c357371682b855d2d79655b41120331b7a/components/camel-schematron).
- [ISO Schematron API](http://schematron.com/schematron-skeleton-api/)
