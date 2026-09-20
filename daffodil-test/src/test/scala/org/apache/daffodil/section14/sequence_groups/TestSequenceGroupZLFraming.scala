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

package org.apache.daffodil.section14.sequence_groups

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestSequenceGroupZLFraming extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section14/sequence_groups/SequenceGroupZLFraming.tdml"
}

class TestSequenceGroupZLFraming extends TdmlTests {
  val tdmlSuite = TestSequenceGroupZLFraming

  // Controls. Each is identical to its bug partner but with no delimiter on the
  // inner model group.
  @Test def ctl_absent = test
  @Test def ctl_present = test
  @Test def ctlChoice_absent = test
  @Test def ctl_absent_unparse = test

  // The %ES; initiator is fine when the group is present.
  @Test def esInit_present = test

  // DAFFODIL-2132. A trailing group whose only delimiter matches nothing must
  // not require the separator on parse, nor write one on unparse.
  //
  // Section 12.2 makes a bare %ES; terminator a Schema Definition Error where
  // the parser scans for delimiters. Daffodil does not diagnose that, so the
  // terminator cases below assert parse behavior for such a schema.
  @Test def esInit_absent = test
  @Test def esTerm_absent = test
  @Test def wspInit_absent = test
  @Test def esChoice_absent = test
  @Test def esInit_absent_unparse = test

  // List and expression forms. The zero-length rules apply per list item, so
  // "X %ES;" and "%WSP*;%WSP*;" match nothing too. A delimiter computed at
  // runtime cannot be analyzed, so it is not known to occupy bits.
  @Test def esInitList = test
  @Test def wspWspInit = test
  @Test def esTermList = test
  @Test def exprTermReal = test
  @Test def constTermReal = test

  // The unparse side of the same cases, which is not symmetric with parse. A
  // delimiter list emits its first literal on unparse, so "X %ES;" writes "X"
  // and the group is not zero length, hence these expect a separator. Only
  // "%WSP*;%WSP*;" writes nothing, so only it suppresses one.
  @Test def esInitList_unparse = test
  @Test def wspWspInit_unparse = test
  @Test def esTermList_unparse = test
  @Test def exprTermReal_unparse = test
}
