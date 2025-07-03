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

Daffodil Validator API implementation for ISO Schematron in XSLT2.

Supports standalone sch files or embedded schematron rules within the Daffodil schema.

### Configuration

#### Parameters

- `schematron`: default parameter provides path to the schematron file or resource

#### XSLT & XPATH versions

Schematron defaults to 1.0 of both XSLT and XPath. Use 2.0 by setting `sch:queryBinding="xslt2"` on the schema.

See version support comments in the [XSL](src/main/resources/iso-schematron-xslt2/iso_schematron_skeleton_for_saxon.xsl)
for more details.

### Parse Results

- `SchematronValidationError` on failed rule check

### SPI registration

Registered as `schematron`

`Validators.getInstance().get("schematron")`

### Command Line Usage

The `schematron` argument to the `--validate` flag will provide the schematron parameter and initiate the parsing.

`daffodil parse --validate schematron=my.sch ...`

### References

- [ISO Schematron API](http://schematron.com/schematron-skeleton-api/)
- [Query language support](https://github.com/Schematron/schematron/blob/2020-10-01/trunk/schematron/code/iso_schematron_skeleton_for_saxon.xsl#L120-L156)
- [Camel Schematron Component](https://github.com/apache/camel/tree/4ea9e6c357371682b855d2d79655b41120331b7a/components/camel-schematron).
- [Combining Schematron with other XML Schema languages](http://www.topologi.com/resources/schtrn_xsd_paper.html)
