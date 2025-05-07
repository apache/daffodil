/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import sbt._

object Rat {

  lazy val excludes: Seq[sbt.File] = Seq(
    // git files
    file(".git"),

    // IntelliJ files
    file(".idea"),

    // version file does not contain a license header
    file("VERSION"),

    // UTF-16BE, Apache Rat thinks it is a binary and cannot tell it includes the Apache v2 license
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_09.dfdl.xsd"
    ),

    // images used for the windows installer
    file("daffodil-cli/src/windows/apache-daffodil.ico"),
    file("daffodil-cli/src/windows/dialog.bmp"),

    // generated code examples
    file("daffodil-codegen-c/src/test/examples"),

    // test files that cannot include the Apache license without breaking tests
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/hextest.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input1.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input2.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input3.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input4.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input5.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input6.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input7.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input8.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input9.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input10.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input11.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input12.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input13.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input14.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input14.exi"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input14.exisa"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input15.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input16.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input18.json"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input18.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input18.exi"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input18.exisa"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/input19.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/inputBig1M.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/prefix.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/test_DFDL-714.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/input/uuid.txt"),
    file("daffodil-codegen-c/src/test/resources/org/apache/daffodil/codegen/c/data"),
    file("daffodil-core/src/test/resources/iso8859.doc.dat"),
    file("daffodil-core/src/test/resources/test/api/01very_simple.txt"),
    file("daffodil-core/src/test/resources/test/api/myData.dat"),
    file("daffodil-core/src/test/resources/test/api/myData2.dat"),
    file("daffodil-core/src/test/resources/test/api/myData3.dat"),
    file("daffodil-core/src/test/resources/test/api/myData4.dat"),
    file("daffodil-core/src/test/resources/test/api/myData5.dat"),
    file("daffodil-core/src/test/resources/test/api/myData16.dat"),
    file("daffodil-core/src/test/resources/test/api/myData19.dat"),
    file("daffodil-core/src/test/resources/test/api/myDataBroken.dat"),
    file("daffodil-tdml-lib/src/test/resources/test/tdml/test.bin"),
    file("daffodil-tdml-lib/src/test/resources/test/tdml/test.txt"),
    file("daffodil-tdml-processor/src/test/resources/test/tdml/test.bin"),
    file("daffodil-tdml-processor/src/test/resources/test/tdml/test.txt"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/codegen/c/data"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/ext_file.txt"),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/section00/general/ext_file2.txt"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_01.bin"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_02.bin"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_03.bin"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_04.bin"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_07.bin"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_13a.bin"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_13b.bin"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_13c.bin"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_13d.bin"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/02nine_headers.txt"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_01.dat"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_01.dat.xml"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_01.dat.xml.dat"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_02.dat"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_02.xml"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_03.dat"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_04.xml"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_05.xml"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_06.xml"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_07.xml"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_08.dat"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/nonamespace/binMessage_01.dat"
    ),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/nonamespace/binMessage_01.dat.xml"
    ),
    file("daffodil-test/src/test/resources/org/apache/daffodil/usertests/Book2.csv"),
    file(
      "daffodil-test/src/test/resources/org/apache/daffodil/usertests/test_prefix_separator_as_variable"
    ),
    file("daffodil-test/src/test/resources/test space/A BTinyData.tdml.dat"),
    file(
      "daffodil-tdml-processor/src/test/resources/test/tdml/fake-precompiled-dfdl-schema.bin"
    ),
    file("test-stdLayout/src/test/resources/org1/test-outer-data1.txt"),
    file("test-stdLayout/src/test/resources/org2/test-data1.txt"),
    file("daffodil-cli/src/test/resources/org/apache/daffodil/cli/debug.txt")
  )

  lazy val BSD2_LICENSE_NAME = "BSD 2-Clause License"

  lazy val LICENSE_TEXT_PASSERA =
    """
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""

}
