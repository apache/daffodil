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

  lazy val excludes = Seq(

    // git files
    file(".git"),

    // scaladoc related, has no way to include a license
    file("daffodil-sapi/root-doc.txt"),

    // UTF-16BE, Apache Rat thinks it is a binary and cannot tell it includes the Apache v2 license
    file("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_09.dfdl.xsd"),

    // images used for the windows installer
    file("daffodil-cli/src/windows/apache-daffodil.ico"),
    file("daffodil-cli/src/windows/banner.bmp"),
    file("daffodil-cli/src/windows/dialog.bmp"),

    // generated_code.[ch] examples
    file("daffodil-runtime2/src/test/c/examples"),

    // test files that cannot include the Apache license without breaking tests
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/hextest.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input2.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input3.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input4.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input5.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input6.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input7.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input8.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input9.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input10.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input11.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input12.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input13.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input14.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input14.exi"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input14.exisa"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input15.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input16.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.json"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.exi"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.exisa"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input19.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/inputBig1M.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/prefix.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/test_DFDL-714.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/uuid.txt"),
    file("daffodil-io/src/test/resources/iso8859.doc.dat"),
    file("daffodil-japi/src/test/resources/test/japi/01very_simple.txt"),
    file("daffodil-japi/src/test/resources/test/japi/myData.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myData2.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myData3.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myData4.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myData5.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myData16.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myData19.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myDataBroken.dat"),
    file("daffodil-runtime2/src/test/resources/org/apache/daffodil/runtime2/ex_nums.dat"),
    file("daffodil-runtime2/src/test/resources/org/apache/daffodil/runtime2/ex_nums.error.dat"),
    file("daffodil-runtime2/src/test/resources/org/apache/daffodil/runtime2/nested.struct.dat"),
    file("daffodil-runtime2/src/test/resources/org/apache/daffodil/runtime2/nested.union.bar.dat"),
    file("daffodil-runtime2/src/test/resources/org/apache/daffodil/runtime2/nested.union.foo.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/01very_simple.txt"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData2.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData3.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData4.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData5.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData16.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData19.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myDataBroken.dat"),
    file("daffodil-tdml-lib/src/test/resources/test/tdml/test.bin"),
    file("daffodil-tdml-lib/src/test/resources/test/tdml/test.txt"),
    file("daffodil-tdml-processor/src/test/resources/test/tdml/test.bin"),
    file("daffodil-tdml-processor/src/test/resources/test/tdml/test.txt"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/ISRM_green_to_orange_60000.0.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/ISRM_green_to_orange_60000.1.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/ISRM_orange_to_green_60002.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/MPU_green_to_orange_60004.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/MPU_orange_to_green_60006.0.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/MPU_orange_to_green_60006.1.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/collisions.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/egress_xdcc_bw.11.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/egress_xdcc_bw.12.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/egress_xdcc_bw.13.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/egress_xdcc_bw.14.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/egress_xdcc_bw.15.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/egress_xdcc_bw.16.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/egress_xdcc_bw.17.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/egress_xdcc_bw.18.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/egress_xdcc_bw.19.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/egress_xdcc_bw.20.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/ingress_xdcc_bw.111.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/ingress_xdcc_bw.112.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/ingress_xdcc_bw.113.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/ingress_xdcc_bw.114.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/ingress_xdcc_bw.115.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/ingress_xdcc_bw.116.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/orion.aptina.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/orion.camera.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/orion.command.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/orion.limits.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/runtime2/orion.video.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/ext_file.txt"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/ext_file2.txt"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_01.bin"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_02.bin"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_03.bin"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_04.bin"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_07.bin"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_13a.bin"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_13b.bin"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_13c.bin"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section05/simple_types/blobs/blob_13d.bin"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/02nine_headers.txt"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_01.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_01.dat.xml"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_01.dat.xml.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_02.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_02.xml"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_03.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_04.xml"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_05.xml"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_06.xml"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_07.xml"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/namespaced/binMessage_08.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/nonamespace/binMessage_01.dat"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/infoset/stringAsXml/nonamespace/binMessage_01.dat.xml"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/usertests/Book2.csv"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/usertests/test_prefix_separator_as_variable"),
    file("daffodil-test/src/test/resources/test space/A BTinyData.tdml.dat"),
    file("daffodil-tdml-processor/src/test/resources/test/tdml/fake-precompiled-dfdl-schema.bin"),
    file("test-stdLayout/src/test/resources/org1/test-outer-data1.txt"),
    file("test-stdLayout/src/test/resources/org2/test-data1.txt"),
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
