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

package org.apache.daffodil.section15.choice_groups

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestChoice1765 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section15/choice_groups/choice1765.tdml"
}

object TestChoice1773 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section15/choice_groups/choice1773.tdml"
}

object TestChoice2162 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section15/choice_groups/choice2162.tdml"
}

object TestChoice2736 extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section15/choice_groups/choice2736.tdml"
}

class TestChoice1765 extends TdmlTests {
  val tdmlSuite = TestChoice1765

  // DFDL-1765
  @Test def backtrack1 = test
  @Test def backtrack2 = test
  @Test def backtrack3 = test
  @Test def backtrack4 = test
}

class TestChoice1773 extends TdmlTests {
  val tdmlSuite = TestChoice1773

  // DFDL-1773
  @Test def choiceSlotAmbiguous1 = test
  @Test def choiceSlotAmbiguous2 = test

  // DAFFODIL-1773
  @Test def queryStyle1 = test
  @Test def queryStyle2 = test
}

class TestChoice2162 extends TdmlTests {
  val tdmlSuite = TestChoice2162

  // DAFFODIL-2162
  @Test def choiceArrayDirectDispatch1 = test
}

class TestChoice2736 extends TdmlTests {
  val tdmlSuite = TestChoice2736

  // DAFFODIL-2736
  @Test def choiceAmbiguousUPA = test
}
