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

package org.apache.daffodil.sapi

/**
 * Provides the classes necessary to perform parse tracing or create a custom debugger
 *
 * <h3>Overview</h3>
 *
 * Daffodil comes with one prebuilt debugger, the [[TraceDebuggerRunner]], which outputs
 * verbose information during the parsing processes, which can be used to aid
 * in debugging a DFDL schema. For example, the [[TraceDebuggerRunner]] can be use like so:
 *
 * {{{
 * val tdr = new TraceDebuggerRunner()
 * Daffodil.setDebugger(tdr)
 * }}}
 *
 * Additionally, one may create their own debugger runner by implementing the
 * methods in the [[DebuggerRunner]].
 * <p>
 * Once the debugger is set, it must then be turned on, like so:
 *
 * {{{
 * Daffodil.setDebugging(true);
 * }}}
 */

package object debugger
