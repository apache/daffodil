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
    file(".git"),
    file(".keep"),

    // eclispe related files
    file(".classpath"),
    file(".project"),
    file(".jupiter"),
    file(".ensime"),

    // scaladoc related, has no way to include a license
    file("daffodil-sapi/root-doc.txt"),

    // UTF-16BE, Apache Rat thinks it is a binary and cannot tell it includes the Apache v2 license
    file("daffodil-test/src/test/resources/org/apache/daffodil/section06/namespaces/multi_base_09.dfdl.xsd"),

    // test files that cannot include the Apache license without breaking tests
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1326"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1328"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1329"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1330"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1331"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1333"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1334"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1337"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1338"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1339"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1340"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1382"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1591"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1602"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/1863"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/debugger/982"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/hextest.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input10.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input11.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input12.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input13.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input14.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input15.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input16.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input18.json"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input19.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input1.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input2.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input3.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input4.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input5.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input6.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input7.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input8.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/input9.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/inputBig1M.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/input/test_DFDL-714.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output10.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output11.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output12.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output13.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output14.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output15.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output16.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output1_nopretty.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output1.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output2.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output3.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output4.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output5.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output6.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output8.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output9.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/output_DFDL-714.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/unqualified_path_step_01.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/unqualified_path_step_02.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/unqualified_path_step_03.txt"),
    file("daffodil-cli/src/it/resources/org/apache/daffodil/CLI/output/unqualified_path_step_04.txt"),
    file("daffodil-io/src/test/resources/iso8859.doc.dat"),
    file("daffodil-japi/src/test/resources/test/japi/01very_simple.txt"),
    file("daffodil-japi/src/test/resources/test/japi/myData16.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myData2.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myData3.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myData4.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myData5.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myDataBroken.dat"),
    file("daffodil-japi/src/test/resources/test/japi/myData.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/01very_simple.txt"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData16.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData2.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData3.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData4.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData5.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myDataBroken.dat"),
    file("daffodil-sapi/src/test/resources/test/sapi/myData.dat"),
    file("daffodil-tdml-lib/src/test/resources/test/tdml/test.bin"),
    file("daffodil-tdml-lib/src/test/resources/test/tdml/test.txt"),
    file("daffodil-tdml-processor/src/test/resources/test/tdml/test.bin"),
    file("daffodil-tdml-processor/src/test/resources/test/tdml/test.txt"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/ext_file2.txt"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section00/general/ext_file.txt"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/section06/entities/02nine_headers.txt"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/usertests/test_prefix_separator_as_variable"),
    file("daffodil-test/src/test/resources/org/apache/daffodil/usertests/Book2.csv"),
    file("daffodil-test/src/test/resources/test space/A BTinyData.tdml.dat"),
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
