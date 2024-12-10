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

package org.apache.daffodil.junit

/**
 * <h3>Overview</h3>
 *
 * Provides two traits to reduce boilerplate related to defining TDML tests:
 *
 * [[TdmlSuite]]: mixed into a Junit test suite companion object ensures the TDML Runner is
 * created and cleaned up appropriately. Direct access to the runner is available, though most
 * tests should use the "test" or "trace" methods defined in [[TdmlTests]]. Note that a
 * TdmlSuite only supports a single TDML file and runner. If multiple are needed, additional
 * test class objects must be created. The TdmlSuite also only supports creating a Runner with
 * the default arguments. This is sufficient in most cases, but if a different runner is needed,
 * implementations can override the [[TdmlSuite.createRunner]] function.
 *
 * [[TdmlTests]]: contains [[TdmlTests.test]] and [[TdmlTests.trace]] helper functions to easily
 * run or trace tests without having to provide the runner or the test name. The JUnit test name
 * is used as the parameter passed to runOneTest (i.e. the name of the test in the TDML file).
 * The runner to use is the runner associated with the companion object, which must be set in
 * the [[TdmlTests.tdmlSuite]] val
 *
 * <h4>Examples</h4>
 *
 * The following example defines a JUnit test suite called "MyTests". The companion object mixes
 * in TdmlSuite and defines the tdmlResoruce to use for all tests in the suite. The companion
 * class mixes in TdmlTests and provides a reference to the companion object via the tdmlSuite
 * val, and defines a number of JUnit tests that each call the "test" method to run the TDML
 * test with the same name as the JUnit test.
 * {{{
 * import org.apache.daffodil.junit.tdml.TdmlSuite
 * import org.apache.daffodil.junit.tdml.TdmlTests
 *
 * import org.junit.Tests
 *
 * object MyTests extends TdmlSuite {
 *   val tdmlResource = "/resource/path/to/tests.tdml"
 * }
 *
 * class MyTests extends TdmlTests {
 *   val tdmlSuite = MyTests
 *
 *   @Test def test1 = test
 *   @Test def test2 = test
 *   @Test def test3 = test
 * }
 *}}}
 */
package object tdml
